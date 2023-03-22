module StearnsWharf.BeamSpec 
  ( spec
  )
where 

import Test.Hspec

import Numeric.LinearAlgebra ((<>),(|>),(#>),Matrix,Vector,fromList,toList) 
-- ,fromLists,disp,dispf,tr)
import Numeric.LinearAlgebra.Data (flatten)
-- import Data.Vector.Storable (basicLength)

import StearnsWharf.Node hiding (calcGeom)
import StearnsWharf.Load
import StearnsWharf.Beam
import StearnsWharf.WoodProfile
import StearnsWharf.Material

diffLists :: [Double] -> [Double] ->[Double]
diffLists [] _ = []
diffLists _ [] = []
diffLists (x:xs) (y:ys) = abs (x - y) : diffLists xs ys

eqVec :: Vector Double -> Vector Double -> Bool
eqVec v1 v2 = 
  let 
    v1' = toList v1
    v2' = toList v2
  in
  if length v1' /= length v2' then
    False 
  else
    let 
      diffResult = diffLists v1' v2'
    in
    all (\x -> x < 0.011)  diffResult

no1 :: Node
no1 = 
  Node 
  { nodeId = "n1"
  , nx = 0.0
  , ny = 0.0
  , dof = Dof 0 0 1
  , globNdx = 0
  } 

no12 :: Node
no12 = 
  Node 
  { nodeId = "n12"
  , nx = 10.0
  , ny = 0.0
  , dof = Dof 0 0 1
  , globNdx = 1
  } 

no13 :: Node
no13 = 
  Node 
  { nodeId = "n13"
  , nx = 0.0
  , ny = 10.0
  , dof = Dof 0 0 1
  , globNdx = 1
  } 

no14 :: Node
no14 = 
  Node 
  { nodeId = "n14"
  , nx = 10.0
  , ny = 0.0
  , dof = Dof 0 0 1
  , globNdx = 1
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

c24 :: WoodProfile
c24 = createWoodProfile TEST_CLASS 100 200

beam1 :: Beam WoodProfile
beam1 = 
  let 
    bid = BeamId "beam1"
    fn = (FirstNode no1)
    sn = (SecondNode no12)
    load = Nothing 
    profile = c24
  in
  (Bjlk33 bid fn sn profile load)

beam2 :: Beam WoodProfile
beam2 = 
  let 
    bid = BeamId "beam2"
    fn = (FirstNode no1)
    sn = (SecondNode no13)
    load = Nothing 
    profile = c24 
  in
  (Bjlk33 bid fn sn profile load)

beam3 :: Beam WoodProfile
beam3 = 
  let 
    bid = BeamId "beam3"
    fn = (FirstNode no1)
    sn = (SecondNode no2)
    load = Nothing 
    profile = c24 
  in
  (Bjlk33 bid fn sn profile load)


sy1 :: Vector Double
sy1 = fromList [0.0,50.0,-83.33,0.0,50.0,83.33]

sy2 :: Vector Double
sy2 = fromList [0.0,35.0,-50.0,0.0,15.0,33.33]

sy3 :: Vector Double
sy3 = fromList [-50.0,0.0,-83.33,-50.0,0.0,83.33]

sy4 :: Vector Double
sy4 = fromList [-12.50,21.65,-20.83,-12.50,21.65,20.83]

{-
beam4 :: Beam WoodProfile
beam4 = 
  let 
    bid = BeamId "beam4"
    fn = (FirstNode no1)
    sn = (SecondNode no14)
    load = Nothing 
    profile = c24 
  in
  (Bjlk32 bid fn sn profile load)

expectedBjlk32 :: Vector Double
expectedBjlk32 = 
  fromList
   [0.0,0.0,0.0,-0.0,0.0,0.0,0.0,0.0,-0.0,0.0,-0.0,-0.0,0.0,-0.0,
   0.0,0.0,0.0,0.0,-0.0,0.0,0.0,0.0,0.0,0.0,0.0,-0.0,0.0,0.0,0.0,
   0.0,0.0,-0.0,0.0,0.0,0.0,0.0]
-}

spec :: Spec
spec = do
  describe "BeamSpec" $ do
    context "eqVec" $ do
      it "tv1 and tv2 equal" $ do
        shouldBe (eqVec tv1 tv2) True
      it "tv1 and tv3 not equal" $ do
        shouldBe (eqVec tv1 tv3) False
      it "tv1 and tv4 not equal" $ do
        shouldBe (eqVec tv1 tv4) False
    context "createSY" $ do
      let geom1 = calcGeom beam1
      it "local coordinates" $ do
        let syResult = createSY geom1 load1
        shouldBe (eqVec syResult sy1) True
        let syResult2 = createSY geom1 load2
        shouldBe (eqVec syResult2 sy2) True
      it "global coordinates beam1" $ do
        let syResult_ = createSY_ geom1 load1
        shouldBe (eqVec syResult_ sy1) True
      it "global coordinates beam2" $ do
        let geom2 = calcGeom beam2
        let syResult2_ = createSY_ geom2 load1
        shouldBe (eqVec syResult2_ sy3) True
      it "global coordinates beam3" $ do
        let geom3 = calcGeom beam3
        let syResult3_ = createSY_ geom3 load1
        shouldBe (eqVec syResult3_ sy4) True
      



{-     context "createK Bjlk32" $ do
      it "stiffness values" $ do
        let k32 = flatten $ createK beam4
         shouldBe k32 expectedBjlk32
-}

        