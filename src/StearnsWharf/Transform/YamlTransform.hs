{-# LANGUAGE DeriveGeneric,CPP,OverloadedStrings,NamedFieldPuns,RecordWildCards,StrictData #-}

module StearnsWharf.Transform.YamlTransform 
where

import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Data.Yaml 
  ( FromJSON(..)
  , ParseException
  , decodeFileEither
  )
import StearnsWharf.System 
  ( System(..)
  , emptySystem
  )
import StearnsWharf.Beam (Beam(..))
import StearnsWharf.Load(Load(..), PointLoad(..))
import StearnsWharf.Node (Node(..))
import StearnsWharf.WoodProfile(WoodProfile(..))

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
  , lx :: Double
  , ly :: Double
  } deriving (Show,Eq,Generic)

data YamlPointLoad = 
  YamlPointLoad
  { pid :: Int
  , pf :: Double
  , v  :: Double
  , ang :: Int
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

instance FromJSON B33 
instance FromJSON WoodProfileInternal
instance FromJSON YamlWoodProfile
instance FromJSON YamlPointLoad
instance FromJSON YamlLoad
instance FromJSON YamlNode
instance FromJSON YamlSystem 

transformNodes :: YamlSystem -> [Node]
transformNodes ymlsys = 
  []

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

transformYamlToSystem :: YamlSystem -> System
transformYamlToSystem ymlsystem = 
  emptySystem

parseYaml :: FilePath -> IO (Maybe System)
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
