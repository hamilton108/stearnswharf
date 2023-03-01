{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards #-}
module StearnsWharf.System where

-- #define RCS_DEBUG

import Control.Monad.Reader (runReader)

import qualified Text.XML.Light as X 

import Data.Map (elems)

import Numeric.LinearAlgebra ((#>),Matrix,Vector,inv)
import Numeric.LinearAlgebra.Devel (runSTMatrix,runSTVector,newVector,newMatrix)


import qualified StearnsWharf.Nodes as N
import qualified StearnsWharf.Loads as L
import qualified StearnsWharf.Beams as B
import qualified StearnsWharf.Output as OUT

import qualified StearnsWharf.XML.XmlNodes as XN
import qualified StearnsWharf.XML.XmlLoads as XL
import qualified StearnsWharf.XML.XmlProfiles as XP

import qualified StearnsWharf.Wood.WoodProfiles as WP
import qualified StearnsWharf.Steel.SteelProfiles as SP

#ifdef RCS_DEBUG
import qualified StearnsWharf.XML.Common as XC
#endif

data ProfileContext = ProfileContext {
                            steelProfiles :: [B.Beam SP.SteelProfile],
                            woodProfiles :: [B.Beam WP.WoodProfile],
                            pointLoads :: [L.PointLoad],
                            numDof :: Int
                        }

systemDof :: [N.Node] -> Int
systemDof nodes = (N.globNdx maxNode) + (N.numDof . N.dof) maxNode
    where maxNode = maximum nodes

systemSY :: ProfileContext -> L.LoadVariant -> Vector Double
systemSY ProfileContext { steelProfiles,woodProfiles,numDof } loadVar = runSTVector $ do
    v <- newVector 0.0 numDof 
    mapM_ (\x -> B.add2systemSY v loadVar x) steelProfiles
    mapM_ (\x -> B.add2systemSY v loadVar x) woodProfiles
    return v

systemPointLoads :: [L.PointLoad] -> Int -> L.LoadVariant -> Vector Double
systemPointLoads ploads numDof loadVar = runSTVector $ do
    v <- newVector 0.0 numDof 
    let myAdd = L.add2systemPointLoads v loadVar
    mapM_ myAdd ploads
    return v

systemK :: ProfileContext -> Matrix Double
systemK ProfileContext { steelProfiles,woodProfiles,numDof } = runSTMatrix $ do
    m <- newMatrix 0.0 numDof numDof 
    mapM_ (\x -> B.add2systemK m x) steelProfiles
    mapM_ (\x -> B.add2systemK m x) woodProfiles 
    return m

calcDeflections :: ProfileContext -> (Vector Double, Vector Double) 
calcDeflections ctx@ProfileContext { pointLoads,numDof } = (resultForces,resultDeflections)
    where invSysK = inv $ systemK ctx 
          sysSyWo = systemSY ctx L.WoFact 
          sysSyWith = systemSY ctx L.WithFact
          sysPwo = systemPointLoads pointLoads numDof L.WoFact 
          sysPwith = systemPointLoads pointLoads numDof L.WithFact 
          resultForces = invSysK #> (sysSyWo + sysPwo)
          resultDeflections = invSysK #> (sysSyWith + sysPwith)

beamResults :: ProfileContext -> Vector Double -> Vector Double -> [OUT.BeamResult]
beamResults ProfileContext { steelProfiles,woodProfiles } vUltimateLimit vServicabilityLimit = concat [woodResults,steelResults]
    where woodResults | null woodProfiles == True = []
                      | otherwise = map (OUT.collectResult vUltimateLimit vServicabilityLimit) woodProfiles 
          steelResults | null steelProfiles == True = []
                       | otherwise = map (OUT.collectResult vUltimateLimit vServicabilityLimit) steelProfiles 

stearnsWharfResult :: X.Element -> [OUT.BeamResult]
stearnsWharfResult doc = beamResults ctx rf rd
    where loads = XL.createLoads doc
          nodes = XN.createMatstatNodes doc
          numDof = systemDof (elems nodes)
          ploads = XL.createPointLoads nodes doc 
          steels = runReader (XP.createSteelProfiles doc) (nodes,loads)
          woods = runReader (XP.createWoodProfiles doc) (nodes,loads)
          ctx = ProfileContext steels woods ploads numDof
          (rf,rd) = calcDeflections ctx
    
runStearnsWharf :: X.Element -> IO ()
#ifdef RCS_DEBUG
runStearnsWharf doc = do 
    let loads = XL.createLoads doc
    let nodes = XN.createMatstatNodes doc
    let steels = runReader (XP.createSteelProfiles doc) (nodes,loads)
    let woods = runReader (XP.createWoodProfiles doc) (nodes,loads)
    let Just steela = XC.xmlElement "SteelProfiles" doc
    let steelb = XC.xmlElements "DBSteelProfile" steela 
    let Just nodesa = XC.xmlElement "nodes" doc
    let nodesb = XC.xmlElements "node" nodesa 
    --putStrLn $ show xmla
    putStrLn $ show steelb
    putStrLn $ show nodesb 
    --putStrLn $ show loads 
    --putStrLn $ show nodes
    --putStrLn $ show woods
    --putStrLn $ show steels 
    --putStrLn $ show items
#else
runStearnsWharf doc = do 
    let result = stearnsWharfResult doc  
    mapM_ OUT.printResults result
    OUT.printSummary result
    return () 
#endif
