{-# LANGUAGE DeriveGeneric,OverloadedStrings,NamedFieldPuns,RecordWildCards,StrictData #-}

module StearnsWharf.Transform.YamlTransform where

-- import Control.Monad.State (State,runState,get,put)
import Control.Monad.IO.Class (liftIO)
-- import qualified Data.Map as Map
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Yaml 
  ( FromJSON(..)
  , ParseException
  , decodeFileEither
  )
import qualified StearnsWharf.System as S
-- import StearnsWharf.Beam (Beam(..))
import qualified StearnsWharf.Load as L
import qualified StearnsWharf.Node as N 
--import StearnsWharf.WoodProfile(WoodProfile(..))

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
  , ld :: Int
  , n1 :: Int
  , n2 :: Int
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

 {-   ploadId :: LoadId
  , plVal :: Double
  , node :: Node
  , plAngle :: Double
  , plFactor :: Double 
  - { pid: 1, pf: 1.4, v: -55.0, ang: 90, pnode: 2 }

 -}
 
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

parseYaml' :: FilePath -> IO (Either ParseException YamlSystem)
parseYaml' fname = 
  decodeFileEither fname -- "/home/rcs/opt/haskell/stearnswharf/tmp/demo1.yaml"

demoFname :: FilePath
demoFname = "/home/rcs/opt/haskell/stearnswharf/t/resources/wood1.yaml"

demo :: IO ()
demo = 
  parseYaml' demoFname >>= \result ->
    case result of 
      Left err -> 
        putStrLn $ show err
      Right res1 -> 
        putStrLn $ show res1

nodeMap :: [N.Node] -> NodeMap -- Map.Map Int N.Node
nodeMap nodes = 
  let 
    tnodes = map (\x -> (N.nodeId x, x)) nodes
  in
  Map.fromList tnodes

transformYamlToSystem :: YamlSystem -> S.System
transformYamlToSystem ymlsystem = 
  let 
    n = transformNodes (nodes ymlsystem)
    nm = nodeMap n
  in
  S.System
  { S.nodes = n
  , S.loads = transformLoads (loads ymlsystem)
  , S.pointLoads = transformPointLoads nm (pointloads ymlsystem)
  , S.woodProfiles = []
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
