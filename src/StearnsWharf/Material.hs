{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module StearnsWharf.Material where

data StrengthClass
  = C16
  | C22
  | C24
  | CEL30c
  | CEL40c
  | GL32c
  | TEST_CLASS
  deriving (Show)

data Mprop = Mprop
  { emodulus
    , mySigma
    , myTau ::
      Double
  , stClass :: StrengthClass
  }
  deriving (Show)

data MaterialCategory
  = Wood
  | Glulam
  deriving (Show)

data Material
  = Material MaterialCategory Mprop
  deriving (Show)

{-
  | Steel
  { emodulus
  , mySigma
  , myTau :: Double
  }
  | Concrete
  { emodulus :: Double
  } deriving Show
-}

data Stress = Stress
  { sigma
    , tau ::
      Double
  }
  deriving (Show)

{-
newSteel :: String -> Material
newSteel wpid = case wpid of "S355" -> Steel 200000.0 355.0 ((/) 355.0 $ sqrt 2)
                             "S235" -> Steel 200000.0 235.0 ((/) 235.0 $ sqrt 2)
-}
