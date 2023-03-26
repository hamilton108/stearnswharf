{-# LANGUAGE OverloadedStrings,NamedFieldPuns,RecordWildCards,StrictData #-}

module StearnsWharf.Transform.YamlTransformSpec 
  ( spec
  )
where 

import Test.Hspec

import StearnsWharf.Transform.YamlTransform
import StearnsWharf.Node 
import StearnsWharf.Load
import StearnsWharf.Beam
import StearnsWharf.WoodProfile
import StearnsWharf.System

testSystem :: YamlSystem
testSystem = 
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

spec :: Spec
spec = do
  describe "YamlTransformSpec" $ do
    context "context" $ do
      it "it" $ do
        shouldBe 1 (1 :: Int)


