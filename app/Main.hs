module Main where

import System.Environment (getArgs)
import Scanner (findDuplicates)
import UI (launchUI)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir] -> do
            putStrLn "Scanning for duplicates"
            groups <- findDuplicates dir
            if null groups
                then putStrLn "No duplicates found"
                else launchUI groups
        _ -> putStrLn "Usage: cabal run duplicate-finder-exe <full path>"
