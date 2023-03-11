module StearnsWharf.Beam where

import Data.Maybe (fromJust)
import Data.List (find)
import Numeric.LinearAlgebra ((<>),(|>),(#>),Matrix,Vector,fromList,fromLists,disp,dispf,tr)
-- import Data.Packed.ST (STMatrix,modifyMatrix,STVector,modifyVector)
import Numeric.LinearAlgebra.Devel (modifyVector,modifyMatrix,STMatrix,STVector,at',atM')
import Control.Monad.ST (ST)

--import qualified StearnsWharf.Materials as M
import qualified StearnsWharf.Load as L
import qualified StearnsWharf.Node as N
import qualified StearnsWharf.Profile as P
import StearnsWharf.Load(Load(..))
import StearnsWharf.Node (Node(..))
import StearnsWharf.Profile (Profile)

newtype BeamId = BeamSecId String

data Beam a 
  = Bjlk33 
  { beamId :: BeamId 
  , n1,n2 :: Node
  , bt :: a
  , ld :: Maybe Load 
  }

createK :: Profile a => Beam a -> Matrix Double
createK (Bjlk33 _ n1' n2' bt' _) = fromLists [[eal, 0.0, 0.0, -eal, 0.0, 0.0],
                                            [0.0, k11, k12, 0.0,  k14, k15],
                                            [0.0, k12, k22, 0.0,  k24, k25],
                                            [-eal,0.0, 0.0, eal,  0.0, 0.0],
                                            [0.0, k14, k24, 0.0,  k44, k45],
                                            [0.0, k15, k25, 0.0,  k45, k55]]
  where aa = P.area bt'
        ee = P.emodulus bt'
        ii = P.secondAreaMoment bt'
        (N.Geom _ _ len) = N.calcGeom n1' n2'
        eal = ee*aa/len
        b = ee*ii/(len**3.0)
        k11 = 12*b
        k12 = (-6)*b*len
        k14 = (-12)*b
        k15 = k12
        k22 = 4*b*len**2
        k24 = 6*b*len
        k25 = 2*b*len**2
        k44 = 12*b
        k45 = 6*b*len
        k55 = 4*b*len**2