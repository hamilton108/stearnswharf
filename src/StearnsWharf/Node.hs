module StearnsWharf.Node where

import qualified Data.Map as Map

import StearnsWharf.Common 
  ( floatEq
  , Cosine(..)
  , Sine(..)
  )

type NodeId = String 

type Index = Int

-- newtype LocalIndex = LocalIndex Index deriving (Show)

-- newtype GlobalIndex = GlobalIndex Index deriving (Show)

type LocalIndex = Index 

type GlobalIndex = Index

type IndexPair = (LocalIndex,GlobalIndex)

-- data NodeType = IsFirstNode | IsSecondNode deriving Eq

newtype FirstNode = FirstNode Node deriving (Show)

newtype SecondNode = SecondNode Node deriving (Show)

data MatrixCoord = MatrixCoord { row, col :: Index } deriving Show

data Loc2glob = Loc2glob { loc, glob :: MatrixCoord } deriving Show
                
data Loc2globVec = Loc2globVec { locv, globv :: Int } deriving Show

data Dof = Dof { dofX, dofY, dofM :: Int } deriving (Eq,Show)

data Node = 
  Node 
  { nodeId :: NodeId
  , nx :: Double
  , ny :: Double
  , dof :: Dof
  , globNdx :: GlobalIndex
  } deriving (Show)

instance Eq Node where
    (==) n1 n2 = (globNdx n1) == (globNdx n2)

instance Ord Node where
    compare n1 n2 = compare (globNdx n1) (globNdx n2)

type NodeMap = Map.Map NodeId Node

data Geom = 
  Geom 
  { c :: Cosine 
  , s :: Sine 
  , len :: Double 
  } deriving (Show)

instance Eq Geom where
    (==) g1 g2 = 
      let 
        (Cosine c1') = c g1
        (Sine s1') = s g1
        (Cosine c2') = c g2
        (Sine s2') = s g2
        cq = floatEq c1' c2'
        sq = floatEq s1' s2'
        lq = floatEq (len g1) (len g2)
      in
      [True,True,True] == [cq,sq,lq]
      
calcGeom :: FirstNode -> SecondNode -> Geom
calcGeom (FirstNode n1) (SecondNode n2) = 
  let xDelta = (nx n2) - (nx n1) 
      yDelta = (ny n2) - (ny n1) 
      xlen = sqrt (yDelta**2.0 + xDelta**2.0)
      xcos = xDelta / xlen  
      xsin = yDelta / xlen  
  in 
  Geom 
  { c = Cosine xcos
  , s = Sine xsin
  , len = xlen 
  }

parseDof :: String -> Dof 
parseDof s = 
  case s of 
    "0" -> Dof 0 0 0
    "1" -> Dof 1 0 0
    "2" -> Dof 0 1 0 
    "3" -> Dof 1 1 0
    "4" -> Dof 0 0 1 
    "5" -> Dof 1 0 1
    "6" -> Dof 0 1 1
    "7" -> Dof 1 1 1
    _ -> undefined

numDof :: Dof -> Int
numDof d = (dofX d) + (dofY d) + (dofM d) 

bitSum :: Dof -> Int
bitSum (Dof d1 d2 d3) = d1 + 2*d2 + 4*d3

indexSeed :: Dof -> Bool -> GlobalIndex -> [IndexPair]
indexSeed d isFirstNode globalIndex = 
  let 
    startIndex = if isFirstNode == True then 0 else 3
    globalIndexY = globalIndex + (dofX d)
    globalIndexM = globalIndexY + (dofY d)
    xp = (startIndex,globalIndex)
    yp = (startIndex+1,globalIndexY)
    zp = (startIndex+2,globalIndexM)
    bs = bitSum d 
  in
  case bs of 
    0 -> []
    1 -> [xp]
    2 -> [yp]
    3 -> [xp,yp]
    4 -> [zp]
    5 -> [xp,zp]
    6 -> [yp,zp]
    7 -> [xp,yp,zp]
    _ -> undefined

indexSeeds :: FirstNode -> SecondNode -> [IndexPair]
indexSeeds (FirstNode n1) (SecondNode n2) = 
  let 
    ip1 = indexSeed d1 True $ globNdx n1
    ip2 = indexSeed d2 False $ globNdx n2 
    d1 = dof n1
    d2 = dof n2
  in
  foldr (:) ip2 ip1