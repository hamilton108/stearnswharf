module StearnsWharf.SystemSpec
  ( spec
  )
where

import Test.Hspec

import Numeric.LinearAlgebra (Matrix, Vector, disp, dispf, fromList, fromLists, toList, tr, (#>), (<>), (|>))

-- import Data.Vector.Storable (basicLength)

import StearnsWharf.Beam
import StearnsWharf.Common (Height (..), Width (..))
import StearnsWharf.Load (LimitStates (..), Load (..), PointLoad (..))
import qualified StearnsWharf.Load as L
import StearnsWharf.Node
import StearnsWharf.System
import StearnsWharf.WoodProfile

--    , n1 = FirstNode (Node {nodeId = 1, nx = 0.0, ny = 0.0, dof = Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 0})
--    , n2 = SecondNode (Node {nodeId = 2, nx = 1.75, ny = 0.0, dof = Dof {dofX = 1, dofY = 1, dofM = 1}, globNdx = 1})

wp1 :: WoodProfile
wp1 =
  WoodProfile
    { width = Width 90.0
    , height = Height 270.0
    , matr = Material Glulam (MaterialProperties{emodulus = 13500.0, mySigma = 32.0, myTau = 3.5, stClass = GL32c})
    }

bjlk33 :: Int -> FirstNode -> SecondNode -> WoodProfile -> Maybe (LimitStates Load) -> Beam WoodProfile
bjlk33 bid fn sn wp lm =
  Bjlk33 $
    BeamProp
      { beamId = bid
      , n1 = fn
      , n2 = sn
      , bt = wp
      , limitStates = lm
      }

l1 :: Load
l1 =
  Load
    { loadId = 1
    , qx1 = 0.0
    , qz1 = (-10.0)
    , qx2 = 0.0
    , qz2 = (-10.0)
    , loadFactor = 1.4
    }

l2 :: Load
l2 =
  Load
    { loadId = 2
    , qx1 = 0.0
    , qz1 = 0.0
    , qx2 = 0.0
    , qz2 = -23.0
    , loadFactor = 1.4
    }

lm1 :: LimitStates Load
lm1 = L.limitStates l1 

-- lm1 :: LimitStates Load
-- lm1 = LimitStates
--   let
--     uls = Load
--         { loadId = 1
--         , qx1 = 0.0
--         , qz1 = (-10.0)
--         , qx2 = 0.0
--         , qz2 = (-10.0)
--         , loadFactor = 1.4
--         }
--   in
--   L.limitStates uls

-- lm2 :: LimitStates Load
-- lm2 = LimitStates
--   let
--     l = { L.uls = Load {loadId = 2, qx1 = 0.0, qz1 = 0.0, qx2 = 0.0, qz2 = -23.0, loadFactor = 1.4}
--   in
--   L.limitStates l

n11 :: Node
n11 =
  Node
    { nodeId = 1
    , nx = 0.0
    , ny = 0.0
    , dof = Dof{dofX = 0, dofY = 0, dofM = 1}
    , globNdx = 0
    }

n12 :: Node
n12 =
  Node
    { nodeId = 2
    , nx = 5.0
    , ny = 0.0
    , dof = Dof{dofX = 1, dofY = 1, dofM = 1}
    , globNdx = 1
    }

n13 :: Node
n13 =
  Node
    { nodeId = 3
    , nx = 10.0
    , ny = 0.0
    , dof = Dof{dofX = 0, dofY = 0, dofM = 1}
    , globNdx = 4
    }

pl1 :: PointLoad
pl1 =
  PointLoad
    { ploadId = 1
    , plVal = -55.0
    , plAngle = 90.0
    , plFactor = 1.4
    , node =
        Node
          { nodeId = 2
          , nx = 1.75
          , ny = 0.0
          , dof = Dof{dofX = 1, dofY = 1, dofM = 1}
          , globNdx = 1
          }
    }

wpSystem1 :: [Beam WoodProfile]
wpSystem1 =
  [ bjlk33 1 (FirstNode n11) (SecondNode n12) wp1 (Just lm1)
  , bjlk33 2 (FirstNode n12) (SecondNode n13) wp1 (Just lm1)
  ]

testSystem :: [Beam WoodProfile] -> [PointLoad] -> System
testSystem wp pl =
  System
    { nodes = []
    , loads = []
    , pointLoads = pl
    , woodProfiles = wp
    }

spec :: Spec
spec = do
  describe "SystemSpec" $ do
    it "-" $ do
      shouldBe 1 1
