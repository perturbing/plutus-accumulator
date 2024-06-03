module Main where

import RunVerifier (runCheckMembership)
import System.IO (stdout)

main :: IO ()
main = do runCheckMembership stdout
