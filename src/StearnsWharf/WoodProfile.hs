{-# LANGUAGE CPP,NamedFieldPuns, RecordWildCards, StrictData #-}

module StearnsWharf.WoodProfile where
  
import StearnsWharf.Common (StaticMoment(..),Shear(..))
import StearnsWharf.Profile (Profile)
import qualified StearnsWharf.Profile as P
--import StearnsWharf.Material (Material(..),MaterialCategory(..),Mprop(..),StrengthClass(..))
--import qualified StearnsWharf.Material as M

newtype Width = Width Double

newtype Height = Height Double

data StrengthClass = 
  C16 | C22 | C24 | CEL30c | CEL40c | GL32c | TEST_CLASS
  deriving Show

data MaterialProperties = 
  MaterialProperties 
  { emodulus
  , mySigma
  , myTau :: Double
  , stClass :: StrengthClass 
  } deriving (Show)

data WoodCategory = 
  Wood | Glulam deriving (Show)

data Material = 
  Material WoodCategory MaterialProperties 
  deriving (Show)

data WoodProfile = 
  WoodProfile 
  { width :: Double
  , height :: Double
  , matr :: Material 
  }
  deriving Show


createWoodProfile :: 
  StrengthClass 
  -> Double
  -> Double
  -> WoodProfile 
createWoodProfile stc w h = case stc of 
  C16     -> WoodProfile w h (Material Wood (MaterialProperties 8000 16 3.2 stc))
  {-
  C22     -> WoodProfile w h (Wood 10000 22 3.8 stc)
  C24     -> WoodProfile w h (Wood 10000 24 4.0 stc)
  CEL30c  -> WoodProfile w h (Glulam 14000 30 4.0 stc)
  CEL40c  -> WoodProfile w h (Glulam 14000 40 4.0 stc)
  GL32c   -> WoodProfile w h (Glulam 14000 40 4.0 stc)
  -}
  TEST_CLASS -> WoodProfile w h (Material Wood (MaterialProperties 1000 10 1.0 stc))

instance Profile WoodProfile where
  desc              WoodProfile { matr = (Material wcat mp)} = 
                      "Wood Profile: " ++ (show wcat) ++ ", " ++ (show $ stClass mp)
  sigma             wp (StaticMoment moment) = moment / (1000.0 * (P.sectionModulus wp)) 
  tau               wp (Shear shr) = (3.0*shr) / (2000.0 * (P.area wp))
  area              WoodProfile { width,height } = (width * height) / 1000000.0
  emodulus          WoodProfile { matr = (Material _ mp) } = (emodulus mp) * 1000.0
  secondAreaMoment  WoodProfile { width,height } = ((width/1000) * ((height/1000)**3)) / 12.0
  sectionModulus    WoodProfile { width,height } = ((width/1000) * ((height/1000)**2)) / 6.0
  centroid          WoodProfile { height } = height / 2000.0

    {-
    desc _ = "Wood Profile" 
    sigma          _ _ = 0.0
    tau            _ _ = 0.0
    area           _ = 0.0
    emodulus       _ = 0.0
    secondAreaMoment _ = 0.0
    sectionModulus   _ = 0.0
    centroid         _ = 0.0
    -}