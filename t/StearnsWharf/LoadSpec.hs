module StearnsWharf.BeamSpec 
  ( spec
  )
where 

import Test.Hspec

import StearnsWharf.Load

uls1 :: Load
uls1 = Load 1 0.0 0.0 0.0 (-10.0) 1.4

uls2 :: Load
uls2 = Load 2 0.0 (-10.0) 0.0 (-10.0) 1.4

sls1 :: Load
sls1 = Load 1 0.0 0.0 0.0 (-10.0) 1.4

sls2 :: Load
sls2 = Load 2 0.0 (-10.0) 0.0 (-10.0) 1.4

sls3 :: Load
sls3 = Load 2 0.0 (-10.0) 0.0 (-10.0) 1.4

lm1 :: LimitStates
lm1 = LimitStates uls1 sls1

lm2 :: LimitStates
lm2 = LimitStates uls2 sls2

lm3 :: LimitStates
lm3 = LimitStates uls1 sls1

spec :: Spec
spec = do
  describe "LoadSpec" $ do
    context "loads" $ do
      it "loads sls2 and sls3 should be equal" $ do
        shouldBe sls2 sls3
      it "loads sls2 and sls1 should not be equal" $ do
        shouldBe (sls2 == sls1) False
    context "limitStates" $ do
      it "limitStates lm1 and lm3 should be equal" $ do
        shouldBe lm1 lm3 
      it "limitStates lm1 and lm2 should not be equal" $ do
        shouldBe (lm1 == lm2) False