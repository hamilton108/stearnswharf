
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Stearnswharf.Params as PA

main :: IO ()
main = PA.cmdLineParser >>= work

work :: PA.Params -> IO ()
work params = 
    putStrLn (show params) 

