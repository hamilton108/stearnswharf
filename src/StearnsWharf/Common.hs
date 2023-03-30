module StearnsWharf.Common where
  
newtype Cosine = Cosine Double deriving (Show)

newtype Sine = Sine Double deriving (Show)

newtype StaticMoment = StaticMoment Double

newtype Shear = Shear Double deriving (Show) 

newtype Width = Width Double deriving (Show) 

newtype Height = Height Double deriving (Show)

floatEq :: (Ord a, Fractional a) => a -> a -> Bool
floatEq a b
  | (abs (a - b)) <= 0.011 = True
  | otherwise      = False


{- newtype ? = ? Double

instance Eq where 
  (==) (LoadVal lv1) lv2 = floatEq lv1 lv2 -}