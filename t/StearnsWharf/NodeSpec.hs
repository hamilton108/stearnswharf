{-# LANGUAGE OverloadedStrings #-}

module StearnsWharf.NodeSpec
  ( spec
  )
where

import Test.Hspec

import StearnsWharf.Node 

n1 :: Node
n1 = 
  Node 
  { nodeId = "n1"
  , nx = 0.0
  , ny = 0.0
  , dof = Dof 0 0 1
  , globNdx = 0
  } 

n2 :: Node
n2 = 
  Node 
  { nodeId = "n2"
  , nx = 5.0
  , ny = 0.0
  , dof = Dof 1 1 1
  , globNdx = 1
  } 
  
n3 :: Node
n3 = 
  Node 
  { nodeId = "n3"
  , nx = 10.0
  , ny = 0.0
  , dof = Dof 0 0 1
  , globNdx = 2
  } 

n4 :: Node
n4 = 
  Node 
  { nodeId = "n4"
  , nx = 15.0
  , ny = 0.0
  , dof = Dof 0 0 1
  , globNdx = 3
  } 

n5 :: Node
n5 = 
  Node 
  { nodeId = "n5"
  , nx = 20.0
  , ny = 0.0
  , dof = Dof 0 0 1
  , globNdx = 4
  } 

testdof :: Dof
testdof = Dof 0 0 1

spec :: Spec
spec = do
  describe "NodeSpec" $ do
    context "Dof" $ do
      it "1 == Dof 1 0 0" $ do
        let d1 = parseDof "1"
        shouldBe (Dof 1 0 0) d1
        shouldBe 1 (numDof d1)
        shouldBe 1 (bitSum d1)
      it "3 == Dof 1 1 0" $ do
        let d3 = parseDof "3"
        shouldBe (Dof 1 1 0) d3 
        shouldBe 2 (numDof d3)
        shouldBe 3 (bitSum d3)
      it "7 == Dof 1 1 1" $ do
        let d7 = parseDof "7"
        shouldBe (Dof 1 1 1) d7 
        shouldBe 3 (numDof d7)
        shouldBe 7 (bitSum d7)
    context "Node" $ do
      it "indexSeed for (Dof 0 0 1), FirstNode, and globalIndex = 0" $ do
        shouldBe [] (indexSeed testdof FirstNode 0)


        