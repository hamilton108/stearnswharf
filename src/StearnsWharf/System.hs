module StearnsWharf.System where

import StearnsWharf.Beam        (Beam)
import StearnsWharf.Load        (Load, PointLoad)
import StearnsWharf.Node        (Node)
import StearnsWharf.WoodProfile (WoodProfile)


data System 
  = System
  { nodes :: [Node]
  , loads :: [Load]
  , pointLoads :: [PointLoad]
  , woodProfiles :: [Beam WoodProfile]
  }
  
emptySystem :: System
emptySystem = 
  System
  { nodes = []
  , loads = []
  , pointLoads = []
  , woodProfiles = []
  }