{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module StearnsWharf.Demo where

x :: Int
x = 3

test :: Num a => a
test = 42

data Load = Load Int deriving (Show)
data PointLoad = PointLoad Int deriving (Show)

instance Eq Load where
  (==) (Load l1) (Load l2) = l1 == l2

data Load2
  = Load2 Int
  | PointLoad2 Int
  deriving (Show)

data LimitStates a = LimitStates
  { uls :: a
  , sls :: a
  }
  deriving (Show)

class Clonable a where
  asSls :: a -> Load2

instance Clonable Load2 where
  asSls (Load2 v) = Load2 (v * 2)
  asSls (PointLoad2 v) = PointLoad2 (v * 10)

instance (Eq a) => Eq (LimitStates a) where
  (==) ls1 ls2 =
    (uls ls1) == (uls ls2)
      && (sls ls1) == (sls ls2)

dax :: LimitStates Load
dax = LimitStates (Load 3) (Load 45)

dax2 :: LimitStates Load
dax2 = LimitStates (Load 33) (Load 45)

yax :: Load2 -> LimitStates Load2
yax u =
  LimitStates u (asSls u)

data TypeA = TypeA1 | TypeA2 deriving (Show)

class MyClass a where
  foo :: Int -> a

instance MyClass TypeA where
  foo 1 = TypeA1
  foo _ = TypeA2

bar :: (MyClass a) => Int -> a
bar = foo

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
