{-# LANGUAGE OverloadedStrings,NamedFieldPuns,RecordWildCards,StrictData #-}

module StearnsWharf.Transform.YamlTransformSpec 
  ( spec
  )
where 

import Test.Hspec
import Data.List (sort)

import StearnsWharf.Transform.YamlTransform
import StearnsWharf.Node 
import StearnsWharf.Load
import StearnsWharf.Beam
import StearnsWharf.WoodProfile
import StearnsWharf.System
import qualified StearnsWharf.System as System
import qualified StearnsWharf.Node as Node
import qualified StearnsWharf.Transform.YamlTransform as YT

nodeEq :: (Node, Node) -> Bool
nodeEq (n1, n2) = 
  all ((==) True)
  [ Node.nodeId n1 == Node.nodeId n2
  , Node.nx n1 == Node.nx n2
  , Node.ny n1 == Node.ny n2
  , Node.dof n1 == Node.dof n2
  , Node.globNdx n1 == Node.globNdx n2
  ]

testSystemYaml :: YamlSystem
testSystemYaml = 
  YamlSystem 
  { nodes = 
    [ YamlNode {nid = 1, x = 0.0, y = 0.0, dof = 1}
    , YamlNode {nid = 2, x = 1.75, y = 0.0, dof = 7}
    , YamlNode {nid = 3, x = 3.5, y = 0.0, dof = 1}
    , YamlNode {nid = 4, x = 7.0, y = 0.0, dof = 7}
    , YamlNode {nid = 5, x = 10.0, y = 0.0, dof = 1}
    ]
  , loads = 
    [ YamlLoad {lid = 1, f = 1.4, lx = 0.0, ly = -10.0}
    , YamlLoad {lid = 2, f = 1.4, lx = 0.0, ly = -23.0}
    ]
  , pointloads = 
    [ YamlPointLoad {pid = 1, pf = 1.4, v = -55.0, ang = 90, pnode = 2}
    ]
  , woodprofiles = 
    [ YamlWoodProfile 
      { stclass = "GL32c", 
        profiles = 
          [ WoodProfileInternal 
            { w = 90.0
            , h = 270.0, 
            b33 = 
              [ B33 {bid = 1, ld = 1, n1 = 1, n2 = 2}
              , B33 {bid = 2, ld = 1, n1 = 2, n2 = 3}
              ]
            }
          , WoodProfileInternal 
            { w = 115.0
            , h = 270.0
            , b33 = 
              [ B33 {bid = 3, ld = 1, n1 = 3, n2 = 4}
              , B33 {bid = 4, ld = 1, n1 = 4, n2 = 5}
              ]
            }
          ]
        }
      ]
    }

testSystem :: System
testSystem = 
  System
  { nodes = 
    [ Node {nodeId = 1, nx = 0.0,   ny = 0.0, dof = Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 0}
    , Node {nodeId = 2, nx = 1.75,  ny = 0.0, dof = Dof {dofX = 1, dofY = 1, dofM = 1}, globNdx = 1}
    , Node {nodeId = 3, nx = 3.5,   ny = 0.0, dof = Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 4}
    , Node {nodeId = 4, nx = 7.0,   ny = 0.0, dof = Dof {dofX = 1, dofY = 1, dofM = 1}, globNdx = 5}
    , Node {nodeId = 5, nx = 10.0,  ny = 0.0, dof = Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 8}
    ]
  , loads = []
  , pointLoads = []
  , woodProfiles = []
  }

equalNodes :: [Node] -> [Node] -> Bool
equalNodes expected actual = 
  all ((== ) True) $ map nodeEq $ zip expected actual 
  

equalLoads :: [Load] -> [Load] -> Bool
equalLoads expected actual = 
  True

equalSystem :: System -> System -> Bool
equalSystem expected actual = 
  all (\x -> x == True)
  [ equalNodes (System.nodes expected) (System.nodes actual)
  , equalLoads (System.loads expected) (System.loads actual)
  ]
  
spec :: Spec
spec = do
  describe "YamlTransformSpec" $ do
    context "transformYamlToSystem" $ do
      it "transformNodes" $ do
        let actual = sort $ transformNodes testSystemYaml
        shouldBe (equalNodes (System.nodes testSystem) actual) True
      {-
      it "testSystemYaml transformed to testSystem" $ do
        let expected = transformYamlToSystem testSystemYaml
        shouldBe (equalSystem expected testSystem) True
      -}


