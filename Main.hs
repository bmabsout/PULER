module Main where

import System.Environment
import System.Exit
import System.FilePath

import Flow
import Compiler
import Repl

correct = unlines
    [ "x = 3;"
    , "x = 4;"
    , "z = \"test\";"
    , "y = \\x -> let f = 4 in f + x;"
    , "main = x;"
    ]

correct2 = unlines
    [ "x = 3;"
    , "y = \\x -> let f: Int -> Int = \\z -> z * 4 in (f x);"
    , "z = \\x -> if x then 1 else 2;"
    , "main = (y (z True));"
    ]

correct3 = unlines
    [ "fact = fix f \\x ->"
    , "              if x == 0"
    , "              then 1"
    , "              else let nextFac = x * (f (x - 1));"
    , "                       p = (Print (Int2Str nextFac))"
    , "                   in nextFac;"
    , "main = let g = (fact 8) in {};"
    ]

wrong = unlines
    [ "x = \\y -> y;"
    , "x = 4;"
    , "y = \\x -> let f = 4 in f + x;"
    , "main = x;"
    ]

wrong2 = unlines
    [ "x = 4;"
    , "y = x + \"string\";"
    , "main = (x 3);"
    ]

wrong3 = unlines
    [ "fact = fix f \\x ->"
    , "              if x == 0"
    , "              then 1"
    , "              else let nextFac = x * (f (x - 1));"
    , "                       p = (Print nextFac)"
    , "                   in nextFac;"
    , "main = let g = (fact 8) in {};"
    ]


main = do
    args <- getArgs
    case args of
        [] -> repl Compiler.init
        ["repl"] -> repl Compiler.init
        ["repl", f] -> do
            maybe_s <- readFile f >>= Compiler.stepsBreakdown
            case maybe_s of
                Nothing -> exitWith (ExitFailure 1)
                Just (_, ctxt) -> do
                    repl ctxt
        [f] -> do
            maybe_s <- readFile f >>= Compiler.stepsBreakdown
            case maybe_s of
                Nothing -> exitWith (ExitFailure 1)
                Just (s, _) -> do
                    writeFile (takeBaseName f ++ ".py") s
                    exitWith ExitSuccess
