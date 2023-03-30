{-# LANGUAGE DeriveGeneric,OverloadedStrings,NamedFieldPuns,RecordWildCards,StrictData #-}

module StearnsWharf.Transform.YamlTransform where

-- import Control.Monad.State (State,runState,get,put)
import Control.Monad.Reader           
  ( Reader
  , runReader
  , ask
  )
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Yaml 
  ( FromJSON(..)
  , ParseException
  , decodeFileEither
  )
import qualified StearnsWharf.System as S
import qualified StearnsWharf.Load as L
import qualified StearnsWharf.Node as N 
import qualified StearnsWharf.Beam as B
import qualified StearnsWharf.WoodProfile as W
import StearnsWharf.Common (Width(..),Height(..))


data YamlNode = 
  YamlNode
  { nid :: Int
  , x :: Double
  , y :: Double
  , dof :: Int
  } deriving (Show,Eq,Generic)

data YamlLoad =
  YamlLoad 
  { lid :: Int
  , f :: Double
  , ly1 :: Double
  , ly2 :: Double
  } deriving (Show,Eq,Generic)

data YamlPointLoad = 
  YamlPointLoad
  { pid :: Int
  , pf :: Double
  , v  :: Double
  , ang :: Double
  , pnode :: Int
  } deriving (Show,Eq,Generic)

data B33 =
  B33
  { bid :: Int
  , n1 :: Int
  , n2 :: Int
  , ld :: Int
  } deriving (Show,Eq,Generic)

data WoodProfileInternal =
  WoodProfileInternal 
  { w :: Double
  , h :: Double
  , b33 :: [B33]
  } deriving (Show,Eq,Generic)

data YamlWoodProfile =
  YamlWoodProfile 
  { stclass :: String
  , profiles :: [WoodProfileInternal]
  } deriving (Show,Eq,Generic)


data YamlSystem = 
  YamlSystem 
  { nodes :: [YamlNode]
  , loads :: [YamlLoad]
  , pointloads :: [YamlPointLoad]
  , woodprofiles :: [YamlWoodProfile]
  } deriving (Show,Eq,Generic)

type NodeMap = Map.Map Int N.Node

type LoadMap = Map.Map Int L.Load

type REW = Reader Env [B.Beam W.WoodProfile]

type REW2 = Reader Env (B.Beam W.WoodProfile)

data Env = 
  Env 
  { getNodeMap :: NodeMap
  , getLoadMap :: LoadMap
  }

instance FromJSON B33 
instance FromJSON WoodProfileInternal
instance FromJSON YamlWoodProfile
instance FromJSON YamlPointLoad
instance FromJSON YamlLoad
instance FromJSON YamlNode
instance FromJSON YamlSystem 

transformNode :: YamlNode -> Int -> N.Node
transformNode yn globalIndex =
  N.Node (nid yn) (x yn) (y yn) ((N.bitSumToDof . dof) yn) globalIndex

transformNodes :: [YamlNode] -> [N.Node]
transformNodes yns = go yns 0 []
  where 
    go :: [YamlNode] -> Int -> [N.Node] -> [N.Node]
    go [] _ acc = acc
    go (x : xs) globalIndex acc = 
      let 
        newNode = transformNode x globalIndex
        newGlobIndex = globalIndex + ((N.numDof . N.dof) newNode)
        newAcc = newNode : acc
      in
      go xs newGlobIndex newAcc
      
transformLoad :: YamlLoad -> L.Load  
transformLoad yl = 
  L.Load (lid yl) 0.0 (ly1 yl) 0.0 (ly2 yl) (f yl)


transformLoads :: [YamlLoad] -> [L.Load]
transformLoads yls = 
  map transformLoad yls

transformPointLoad :: NodeMap -> YamlPointLoad -> L.PointLoad  
transformPointLoad nm yl = 
  let 
    myNode = Map.lookup (pnode yl) nm
  in 
  case myNode of 
    Just hit ->
      L.PointLoad (pid yl) (v yl) (ang yl) (pf yl) hit 
    Nothing -> 
      undefined

transformPointLoads :: NodeMap -> [YamlPointLoad] -> [L.PointLoad]
transformPointLoads nm ploads = 
  map (transformPointLoad nm) ploads 

loadMap :: [L.Load] -> LoadMap 
loadMap lds = 
  let 
    tlds = map (\x -> (L.loadId x, x)) lds
  in
  Map.fromList tlds


createBeamProp :: NodeMap -> LoadMap -> W.WoodProfile -> B33 -> Maybe (B.BeamProp W.WoodProfile)
createBeamProp nm lm wp b = 
  Map.lookup (n1 b) nm >>= \myn1 ->
  Map.lookup (n2 b) nm >>= \myn2 ->
  let 
    myl = Map.lookup (ld b) lm
  in
  case myl of 
    Nothing ->
      Just $ B.BeamProp (bid b) (N.FirstNode myn1) (N.SecondNode myn2) wp Nothing
    Just myl1 ->
      Just $ B.BeamProp (bid b) (N.FirstNode myn1) (N.SecondNode myn2) wp (Just $ L.limitStates myl1)

transformB33 :: W.WoodProfile -> B33 -> REW2
transformB33 wp b = 
  ask >>= \curEnv -> 
    let 
      nm = getNodeMap curEnv
      lm = getLoadMap curEnv
      prop = createBeamProp nm lm wp b
    in
    case prop of 
      Nothing -> 
        undefined
      Just p -> 
        pure $ B.Bjlk33 p


transformWoodProfile :: String -> WoodProfileInternal -> REW
transformWoodProfile stc wpi =
  let 
    wp = W.createWoodProfile (W.asStrengthClass stc) (Width (w wpi)) (Height (h wpi))
  in
  mapM (transformB33 wp) (b33 wpi) 
  
transformWoodProfiles' :: YamlWoodProfile -> REW 
transformWoodProfiles' yp = 
  mapM (transformWoodProfile (stclass yp)) (profiles yp) >>= \profiles ->
    pure $ concat profiles

transformWoodProfiles :: [YamlWoodProfile] -> REW 
transformWoodProfiles yps = 
  mapM transformWoodProfiles' yps >>= \profiles -> 
    pure $ concat profiles

parseYaml' :: FilePath -> IO (Either ParseException YamlSystem)
parseYaml' fname = 
  decodeFileEither fname -- "/home/rcs/opt/haskell/stearnswharf/tmp/demo1.yaml"


nodeMap :: [N.Node] -> NodeMap 
nodeMap nodes = 
  let 
    tnodes = map (\x -> (N.nodeId x, x)) nodes
  in
  Map.fromList tnodes

transformYamlToSystem :: YamlSystem -> S.System
transformYamlToSystem ymlsystem = 
  let 
    ns = transformNodes (nodes ymlsystem)
    nm = nodeMap ns
    lds = transformLoads (loads ymlsystem)
    ldm = loadMap lds
    env = Env nm ldm
    wp = runReader (transformWoodProfiles (woodprofiles ymlsystem)) env
  in
  S.System
  { S.nodes = ns
  , S.loads = lds 
  , S.pointLoads = transformPointLoads nm (pointloads ymlsystem)
  , S.woodProfiles = wp
  }

parseYaml :: FilePath -> IO (Maybe S.System)
parseYaml fname = 
  parseYaml' fname >>= \ymlsystem -> 
    case ymlsystem of 
      Left err -> 
        (liftIO $ putStrLn $ show err) *> 
        pure Nothing 
      Right result -> 
        pure $ Just $ transformYamlToSystem result 

demoFname :: FilePath
demoFname = "/home/rcs/opt/haskell/stearnswharf/t/resources/wood1.yaml"

demo :: IO ()
demo = 
  parseYaml demoFname >>= \result ->
    case result of 
      Nothing -> 
        putStrLn "OOPS"
      Just res1 -> 
        putStrLn $ show res1

{-
instance FromJSON System where
  parseJSON = Aeson.withObject "System" $ \o -> System 
    <$> o .: "nodes"
-}

--    <*> o .: "x2"


{-
instance ToJSON System where
  toJSON System {..} = Aeson.object
    [ "x1" .= x1 
    , "x2" .= x2
    ]
  toEncoding (System x1 x2) = Aeson.pairs
    ( "x1"
    .= x1 
    <> "x2"
    .= x2 
    )
-}
