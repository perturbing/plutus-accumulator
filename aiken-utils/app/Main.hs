module Main where

import Testing (
    printMembershipTestingVectors,
    printNonMembershipTestingVectors,
 )

main :: IO ()
main = do
    putStrLn "use aiken_bilinear_accumulator/accumulator.{"
    putStrLn "  check_membership, check_nonmembership,"
    putStrLn "  }\n\n"
    mapM_ printMembershipTestingVectors ([1, 2, 3, 4] <> [5, 10 .. 45])
    mapM_ printNonMembershipTestingVectors ([1, 2, 3, 4] <> [5, 10 .. 45])
