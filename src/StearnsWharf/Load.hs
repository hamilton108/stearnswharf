{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards  #-}

module StearnsWharf.Load where

import StearnsWharf.Node (Node(..))

-- newtype LoadId = LoadId String deriving (Show)

import StearnsWharf.Common 
  ( floatEq
  )

type LoadId = Int 

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
    (==) l1 l2 = 
      (ploadId l1) == (ploadId l2) &&
      floatEq (plVal l1) (plVal l2) &&
      floatEq (plAngle l1) (plAngle l2) &&
      floatEq (plFactor l1) (plFactor l2) &&
      (node l1) == (node l2)

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
    (==) l1 l2 = 
      (loadId l1) == (loadId l2) &&
      floatEq (qx1 l1) (qx1 l2) &&
      floatEq (qy1 l1) (qy1 l2) &&
      floatEq (qx2 l1) (qx2 l2) &&
      floatEq (qy2 l1) (qy2 l2) &&
      floatEq (loadFactor l1) (loadFactor l2) 

instance Ord Load where
    compare l1 l2 = compare (loadId l1) (loadId l2)

data LimitStates = 
  LimitStates 
  { uls :: Load
  , sls :: Load
  } deriving (Show)

instance Eq LimitStates where
  (==) ls1 ls2 = 
    (uls ls1) == (uls ls2) &&
    (sls ls1) == (sls ls2) 

limitStates :: Load -> LimitStates
limitStates ul@Load{..} = 
  let 
    sl =  Load loadId  (qx1 / loadFactor) (qy1 / loadFactor) (qx2 / loadFactor) (qy2 / loadFactor) 1.0
  in 
  LimitStates ul sl 
  
