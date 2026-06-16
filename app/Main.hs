module Main (main) where

import System.Environment (getArgs)

import Overlay (analyzeMain, checkMain, guiMain, mkPatchCli, mkRuleCli)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--check"] -> checkMain
        ["--analyze"] -> analyzeMain
        ("--mkpatch" : rest) -> mkPatchCli rest
        ("--mkrule" : rest) -> mkRuleCli rest
        _ -> guiMain
