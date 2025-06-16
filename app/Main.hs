module Main where

import System.Environment (getArgs)
import Scanner (findDups)
import UI (launchUI)


-- Main entry point, expects directory path to scan as an argument
main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir] -> do
            putStrLn "Scanning for duplicates"
            groups <- findDups dir
            if null groups
                then putStrLn "No duplicates found"
                else launchUI groups
        _ -> putStrLn "Usage: cabal run duplicate-finder-exe <full path>"
