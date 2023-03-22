module StearnsWharf.Common where
  
newtype Cosine = Cosine Double deriving (Show)

newtype Sine = Sine Double deriving (Show)

newtype StaticMoment = StaticMoment Double

newtype Shear = Shear Double

floatEq :: (Ord a, Fractional a) => a -> a -> Bool
floatEq a b
  | (abs (a - b)) <= 0.011 = True
  | otherwise      = False