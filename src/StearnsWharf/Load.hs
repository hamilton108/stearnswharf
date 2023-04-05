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
  , qz1 :: Double
  , qx2 :: Double
  , qz2 :: Double
  , loadFactor :: Double 
  }
  deriving Show


instance Eq Load where
  (==) l1 l2 = 
    (loadId l1) == (loadId l2) &&
    floatEq (qx1 l1) (qx1 l2) &&
    floatEq (qz1 l1) (qz1 l2) &&
    floatEq (qx2 l1) (qx2 l2) &&
    floatEq (qz2 l1) (qz2 l2) &&
    floatEq (loadFactor l1) (loadFactor l2) 

instance Ord Load where
  compare l1 l2 = compare (loadId l1) (loadId l2)

{-
data T a where
    D1 :: Int -> T String 
    D2 :: T Bool
    D3 :: (a,a) -> T [a]
 -}
    

data LimitStates a = 
  LimitStates 
  { uls :: a
  , sls :: a
  } 
  deriving (Show)


instance (Eq a) => Eq (LimitStates a) where
  (==) ls1 ls2 = 
    (uls ls1) == (uls ls2) &&
    (sls ls1) == (sls ls2) 

limitStates :: Load -> LimitStates Load
limitStates ul@Load{..} = 
  let 
    sl =  Load loadId  (qx1 / loadFactor) (qz1 / loadFactor) (qx2 / loadFactor) (qz2 / loadFactor) 1.0
  in 
  LimitStates ul sl 
  

limitStatesPt :: PointLoad -> LimitStates PointLoad
limitStatesPt ul@PointLoad{ plVal, plFactor} = 
  let 
    plVal' = plVal / plFactor 
    sl =  ul { plVal = plVal' , plFactor = 1.0 }
  in 
  LimitStates ul sl 

{-   let 
    sl =  Load loadId  (qx1 / loadFactor) (qz1 / loadFactor) (qx2 / loadFactor) (qz2 / loadFactor) 1.0
  in 
  LimitStates ul sl  -}