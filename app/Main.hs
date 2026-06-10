module Main (main) where

import System.Environment (getArgs)

import Overlay (checkMain, guiMain, mkPatchCli)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--check"] -> checkMain
        ("--mkpatch" : rest) -> mkPatchCli rest
        _ -> guiMain
