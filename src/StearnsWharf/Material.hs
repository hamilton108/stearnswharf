module StearnsWharf.Material where

data StrengthClass = 
  C16 | C22 | C24 | CEL30c | CEL40c | GL32c | TEST_CLASS
  deriving Show

data Material = 
  Wood 
  { emodulus
  , mySigma
  , myTau :: Double
  , stClass :: StrengthClass -- ^ Strength Class
  } 
  | Glulam 
  { emodulus
  , mySigma
  , myTau :: Double 
  , stClass :: StrengthClass -- ^ Strength Class
  } 
  | Steel 
  { emodulus
  , mySigma
  , myTau :: Double 
  } 
  | Concrete 
  { emodulus :: Double 
  } deriving Show

data Stress = 
  Stress 
  { sigma
  , tau :: Double 
  } deriving Show

{-
newSteel :: String -> Material
newSteel wpid = case wpid of "S355" -> Steel 200000.0 355.0 ((/) 355.0 $ sqrt 2)
                             "S235" -> Steel 200000.0 235.0 ((/) 235.0 $ sqrt 2)
-}
