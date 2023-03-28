module StearnsWharf.Load where

import StearnsWharf.Node (Node(..))

-- newtype LoadId = LoadId String deriving (Show)

type LoadId = Int -- LoadId String deriving (Show)

data PointLoad 
  = PointLoad 
  { ploadId :: LoadId
  , plVal :: Double
  , plAngle :: Double
  , plFactor :: Double 
  , node :: Node
  }
  deriving Show

instance Eq PointLoad where
    (==) l1 l2 = (ploadId l1) == (ploadId l2)

instance Ord PointLoad where
    compare l1 l2 = compare (ploadId l1) (ploadId l2)

data Load 
  = Load 
  { loadId :: LoadId
  , qx1 :: Double
  , qy1 :: Double
  , qx2 :: Double
  , qy2 :: Double
  , loadFactor :: Double 
  }
  deriving Show


instance Eq Load where
    (==) l1 l2 = (loadId l1) == (loadId l2)

instance Ord Load where
    compare l1 l2 = compare (loadId l1) (loadId l2)
