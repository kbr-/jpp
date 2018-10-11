module Main where

import System.Environment
import Interpreter
import ParHaml

import ErrM


main :: IO ()
main = do
        args <- getArgs
        prog <- case args of fileName:_ -> readFile fileName
                             _ -> getContents
        let res = pProgram $ myLexer prog
        case res of
            Bad s -> putStrLn s
            Ok tree -> execProgram tree
