module Main where

import RunVerifier (runCheckMembership, runCheckNonMembership)
import System.IO (stdout)

main :: IO ()
main = do
    runCheckMembership stdout
    runCheckNonMembership stdout
