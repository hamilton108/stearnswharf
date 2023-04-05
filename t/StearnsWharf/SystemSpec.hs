module StearnsWharf.SystemSpec 
  ( spec
  )
where 

import Test.Hspec

import Numeric.LinearAlgebra ((<>),(|>),(#>),Matrix,Vector,fromList,toList,fromLists,disp,dispf,tr)
-- import Data.Vector.Storable (basicLength)

import StearnsWharf.Node 
import StearnsWharf.Load (Load(..),PointLoad(..),LimitStates(..))
import qualified StearnsWharf.Load as L
import StearnsWharf.Beam
import StearnsWharf.WoodProfile
import StearnsWharf.System
import StearnsWharf.Common (Height(..),Width(..))

woodProfilesSystem :: [ Beam WoodProfile ]
woodProfilesSystem = 
  [ Bjlk33 
    ( BeamProp 
      { beamId = 1
      , n1 = FirstNode (Node {nodeId = 1, nx = 0.0, ny = 0.0, dof = Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 0})
      , n2 = SecondNode (Node {nodeId = 2, nx = 1.75, ny = 0.0, dof = Dof {dofX = 1, dofY = 1, dofM = 1}, globNdx = 1})
      , bt = WoodProfile 
              { width = Width 90.0
              , height = Height 270.0
              , matr = Material Glulam (MaterialProperties {emodulus = 13500.0, mySigma = 32.0, myTau = 3.5, stClass = GL32c})}
              , limitStates = 
                Just 
                  ( LimitStates 
                    { L.uls = Load {loadId = 1, qx1 = 0.0, qz1 = -10.0, qx2 = 0.0, qz2 = -10.0, loadFactor = 1.4}
                    , L.sls = Load {loadId = 1, qx1 = 0.0, qz1 = -7.142857142857143, qx2 = 0.0, qz2 = -7.142857142857143, loadFactor = 1.0}
                    }
                  )
              }
    )
  , Bjlk33 
    ( BeamProp 
      { beamId = 2
      , n1 = FirstNode (Node {nodeId = 2, nx = 1.75, ny = 0.0, dof = Dof {dofX = 1, dofY = 1, dofM = 1}, globNdx = 1})
      , n2 = SecondNode (Node {nodeId = 3, nx = 3.5, ny = 0.0, dof = Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 4})
      , bt = WoodProfile 
              { width = Width 90.0
              , height = Height 270.0
              , matr = Material Glulam (MaterialProperties {emodulus = 13500.0, mySigma = 32.0, myTau = 3.5, stClass = GL32c}
              )}
      , limitStates = 
          Just 
            ( LimitStates 
              { L.uls = Load {loadId = 2, qx1 = 0.0, qz1 = 0.0, qx2 = 0.0, qz2 = -23.0, loadFactor = 1.4}
              , L.sls = Load {loadId = 2, qx1 = 0.0, qz1 = 0.0, qx2 = 0.0, qz2 = -16.42857142857143, loadFactor = 1.0}
              })
      }
    )
  , Bjlk33 
    ( BeamProp 
      { beamId = 3
      , n1 = FirstNode (Node {nodeId = 3, nx = 3.5, ny = 0.0, dof = Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 4})
      , n2 = SecondNode (Node {nodeId = 4, nx = 7.0, ny = 0.0, dof = Dof {dofX = 1, dofY = 1, dofM = 1}, globNdx = 5})
      , bt = WoodProfile 
              { width = Width 115.0
              , height = Height 270.0
              , matr = Material Glulam (MaterialProperties {emodulus = 13500.0, mySigma = 32.0, myTau = 3.5, stClass = GL32c})
              }
      , limitStates = Just (LimitStates 
          { L.uls = Load {loadId = 1, qx1 = 0.0, qz1 = -10.0, qx2 = 0.0, qz2 = -10.0, loadFactor = 1.4}
          , L.sls = Load {loadId = 1, qx1 = 0.0, qz1 = -7.142857142857143, qx2 = 0.0, qz2 = -7.142857142857143, loadFactor = 1.0}
          })
      }
    )
  ]

testSystem :: System
testSystem = 
  System
  { nodes = []
  , loads = []
  , pointLoads = 
      [ PointLoad 
          { ploadId = 1
          , plVal = -55.0
          , plAngle = 90.0
          , plFactor = 1.4
          , node = 
            Node 
              { nodeId = 2
              , nx = 1.75
              , ny = 0.0
              , dof = Dof {dofX = 1, dofY = 1, dofM = 1}, globNdx = 1
              }
          }
      ]
  , woodProfiles = woodProfilesSystem
  }

spec :: Spec
spec = do
  describe "SystemSpec" $ do
      it "-" $ do
        shouldBe 1 1
        