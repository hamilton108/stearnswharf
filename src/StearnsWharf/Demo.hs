{-# LANGUAGE DeriveGeneric,CPP,OverloadedStrings,NamedFieldPuns,RecordWildCards,StrictData #-}

module StearnsWharf.Demo where

x :: Int
x = 3

class Ax a where 
  ux :: a -> Int

data AxData = AxData Int

instance Ax AxData where 
  ux (AxData v) = 2 * v
  
demo2 :: Functor f => f AxData
demo2 = Just $ AxData 3

{-
import GHC.Generics
import Data.Yaml
import qualified Data.Aeson                    as Aeson

data Node = 
  Node
  { nid :: Int
  , x :: Double
  , y :: Double
  , dof :: Int
  } deriving (Show,Eq,Generic)

data Load =
  Load 
  { lid :: Int
  , f :: Double
  , lx :: Double
  , ly :: Double
  } deriving (Show,Eq,Generic)

data PointLoad = 
  PointLoad
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

data WoodProfile =
  WoodProfile 
  { stclass :: String
  , profiles :: [WoodProfileInternal]
  } deriving (Show,Eq,Generic)


data System = 
  System 
  { nodes :: [Node]
  , loads :: [Load]
  , pointloads :: [PointLoad]
  , woodprofiles :: [WoodProfile]
  } deriving (Show,Eq,Generic)

instance FromJSON B33 
instance FromJSON WoodProfileInternal
instance FromJSON WoodProfile
instance FromJSON PointLoad
instance FromJSON Load
instance FromJSON Node
instance FromJSON System 

demo :: IO (Either ParseException System)
demo = 
  decodeFileEither "/home/rcs/opt/haskell/stearnswharf/tmp/demo1.yaml"
-}

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

