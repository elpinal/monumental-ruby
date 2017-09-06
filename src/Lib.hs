module Lib
    ( run
    ) where

import System.Environment
import System.Exit
import System.IO

repoURI :: String
repoURI = "https://github.com/ruby/ruby"

run :: IO ()
run = do
  args <- getArgs
  doCmd args

failWith :: String -> IO ()
failWith msg = hPutStrLn stderr msg >> exitFailure

doCmd :: [String] -> IO ()
doCmd [] = putStrLn usage >> exitFailure
doCmd (name:args) = cmd name args
  where
    cmd "help" = help
    cmd x = \_ -> failWith $ "monumental-ruby: no such command " ++ show x

usage :: String
usage =
  unlines [ "Monumental-ruby is a tool for managing various versions of Ruby."
          , ""
          , "Usage:"
          , ""
          , "        help         show help"
          , ""
          ]

help :: [String] -> IO ()
help [] = putStrLn usage
help [topic] = helpOf topic
  where
    helpOf "help" = putStrLn "usage: monumental-ruby help [topic]"
    helpOf topic = failWith $ "unknown help topic " ++ show topic ++ ". Run 'monumental-ruby help'."
help _ =
  failWith $
    unlines [ "usage: monumental-ruby help command"
            , ""
            , "Too many arguments given."
            ]
