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
  | otherwise = False

floatEq2 :: (Ord a, Fractional a) => a -> a -> a -> Bool
floatEq2 a b tolerance
  | (abs (a - b)) <= tolerance = True
  | otherwise = False

{- newtype ? = ? Double

instance Eq where
  (==) (LoadVal lv1) lv2 = floatEq lv1 lv2 -}

{- | Second Area Moment for a rectangular profile
 b : profilbredde
 h : profilhøyde
-}
samr :: Double -> Double -> Double
samr b h = b * h ** 3 / 12.0

-- | First Area Moment for a rectangular profile
famr :: Double -> Double -> Double
famr b h = b * h ** 2 / 6.0
