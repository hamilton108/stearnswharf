module StearnsWharf.Load where

import StearnsWharf.Node (Node(..))

-- newtype LoadId = LoadId String deriving (Show)

type LoadId = Int -- LoadId String deriving (Show)

data PointLoad 
  = PointLoad 
  { ploadId :: LoadId
  , plVal :: Double
  , node :: Node
  , plAngle :: Double
  , plFactor :: Double 
  }
  deriving Show

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


