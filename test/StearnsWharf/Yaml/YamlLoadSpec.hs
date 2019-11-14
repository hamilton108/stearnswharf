{-# LANGUAGE OverloadedStrings #-}

module StearnsWharf.Yaml.YamlLoadSpec (spec) where

import Data.Yaml as Yaml

import StearnsWharf.Yaml.YamlLoad as YamlLoad

-- import Control.Monad.Reader (runReaderT)

import Test.Hspec

myFileName  :: String 
myFileName = "/home/rcs/opt/haskell/stearnswharf/test/testfeed/test1.yaml"

myYamlDoc = 
spec :: Spec
spec = do
    describe "YamlLoad" $ do
        context "when string-date is 2019-09-20:1568937600" $ do
            it "expiry date should be (Just 1568937600)" $ do
                shouldBe 1 1 
