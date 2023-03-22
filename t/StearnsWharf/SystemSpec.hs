module StearnsWharf.SystemSpec 
  ( spec
  )
where 

import Test.Hspec

import Numeric.LinearAlgebra ((<>),(|>),(#>),Matrix,Vector,fromList,toList,fromLists,disp,dispf,tr)
-- import Data.Vector.Storable (basicLength)

import StearnsWharf.Node hiding (calcGeom)
import StearnsWharf.Load
import StearnsWharf.Beam
import StearnsWharf.WoodProfile
import StearnsWharf.System

no1 :: Node
no1 = 
  Node 
  { nodeId = "n1"
  , nx = 0.0
  , ny = 0.0
  , dof = Dof 0 0 1
  , globNdx = 0
  } 

no2 :: Node
no2 = 
  Node 
  { nodeId = "n2"
  , nx = 4.33
  , ny = 2.5
  , dof = Dof 1 1 1
  , globNdx = 1
  } 

no3 :: Node
no3 = 
  Node 
  { nodeId = "n3"
  , nx = 8.66
  , ny = 5.0
  , dof = Dof 0 0 1
  , globNdx = 4
  } 

tv1 :: Vector Double
tv1 = fromList [2.3,3.0,0.0,(-4.0),3.2,0.0]

tv2 :: Vector Double
tv2 = fromList [2.3,3.0,0.0,(-4.0),3.2,0.0]

tv3 :: Vector Double
tv3 = fromList [2.4,3.0,0.0,(-4.0),3.2,0.0]

tv4 :: Vector Double
tv4 = fromList []

load1 :: Load
load1 = (Load 0.0 (-10.0) 0.0 (-10.0) 1.5) 

load2 :: Load
load2 = (Load 0.0 (-10.0) 0.0 0.0 1.5) 

beam1 :: Beam WoodProfile
beam1 = 
  let 
    bid = BeamId "beam1"
    fn = (FirstNode no1)
    sn = (SecondNode no12)
    load = Nothing 
    profile = WoodProfile
  in
  (Bjlk33 bid fn sn profile load)

beam2 :: Beam WoodProfile
beam2 = 
  let 
    bid = BeamId "beam2"
    fn = (FirstNode no1)
    sn = (SecondNode no13)
    load = Nothing 
    profile = WoodProfile
  in
  (Bjlk33 bid fn sn profile load)

beam3 :: Beam WoodProfile
beam3 = 
  let 
    bid = BeamId "beam3"
    fn = (FirstNode no1)
    sn = (SecondNode no2)
    load = Nothing 
    profile = WoodProfile
  in
  (Bjlk33 bid fn sn profile load)

{-
beamWload :: Beam WoodProfile -> Load -> Beam WoodProfile
beamWload b l = 
  b { ld = Just l }
-}

sy1 :: Vector Double
sy1 = fromList [0.0,50.0,-83.33,0.0,50.0,83.33]

sy2 :: Vector Double
sy2 = fromList [0.0,35.0,-50.0,0.0,15.0,33.33]

sy3 :: Vector Double
sy3 = fromList [-50.0,0.0,-83.33,-50.0,0.0,83.33]

sy4 :: Vector Double
sy4 = fromList [-12.50,21.65,-20.83,-12.50,21.65,20.83]

spec :: Spec
spec = do
  describe "SystemSpec" $ do
      it "-" $ do
        shouldBe 1 1
        