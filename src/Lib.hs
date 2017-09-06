module Lib
    ( run
    ) where

import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

repoURI :: String
repoURI = "https://github.com/ruby/ruby"

getRootPath :: IO FilePath
getRootPath = do
  home <- getHomeDirectory
  return $ home </> ".monumental-ruby"

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
    cmd x = const $ failWith $ "monumental-ruby: no such command " ++ show x

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

getDest :: String -> IO FilePath
getDest version = do
  root <- getRootPath
  return $ foldl1 combine [root, "repo", version]

clone :: String -> IO ()
clone version = do
  dest <- getDest version
  exists <- doesDirectoryExist dest
  unless exists $
         callProcess "git" ["clone", "--depth", "1", "--branch", version, repoURI, dest]
