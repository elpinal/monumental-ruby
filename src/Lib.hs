module Lib where

import Control.Exception.Safe
import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Files
import System.Process

repoURI :: String
repoURI = "https://github.com/ruby/ruby"

getRootPath :: IO FilePath
getRootPath = flip combine ".monumental-ruby" <$> getHomeDirectory 

run :: IO ()
run = do
  root <- getRootPath
  args <- getArgs
  doCmd root args

failWith :: String -> IO ()
failWith msg = hPutStrLn stderr msg >> exitFailure

type Command = FilePath -> [String] -> IO ()

doCmd :: Command
doCmd _ [] = putStrLn usage >> exitFailure
doCmd root (name:args) = cmd name root args
  where
    cmd :: String -> Command
    cmd "install" = install
    cmd "uninstall" = uninstall
    cmd "use" = use
    cmd "list" = list
    cmd "help" = help
    cmd x = nocmd x

nocmd :: String -> Command
nocmd x _ _ = failWith $ "monumental-ruby: no such command " ++ show x

usage :: String
usage =
  unlines [ "Monumental-ruby is a tool for managing various versions of Ruby."
          , ""
          , "Usage:"
          , ""
          , "        install      install specified versions of Ruby"
          , "        uninstall    uninstall specified versions of Ruby"
          , "        use          select the specific version of Ruby as cureent version"
          , "        list         list installed versions of Ruby"
          , "        help         show help"
          , ""
          ]

help :: Command
help _ [] = putStrLn usage
help _ [topic] = helpOf topic
  where
    helpOf :: String -> IO ()
    helpOf "install" = putStrLn "usage: monumental-ruby install versions..."
    helpOf "uninstall" = putStrLn "usage: monumental-ruby uninstall versions..."
    helpOf "use" = putStrLn "usage: monumental-ruby use version"
    helpOf "list" = putStrLn "usage: monumental-ruby list"
    helpOf "help" = putStrLn "usage: monumental-ruby help [topic]"
    helpOf topic = failWith $ "unknown help topic " ++ show topic ++ ". Run 'monumental-ruby help'."
help _ _ =
  failWith $
    unlines [ "usage: monumental-ruby help command"
            , ""
            , "Too many arguments given."
            ]

install :: Command
install _ [] = failWith "install: 1 or more arguments required"
install root versions = do
  createDirectoryIfMissing True $ root </> "repo"
  mapM_ (clone root) versions
  mapM_ (build root) versions

getDest :: FilePath -> String -> FilePath
getDest root version = foldl1 combine [root, "repo", version]

clone :: FilePath -> String -> IO ()
clone root version = do
  let dest = getDest root version
  exists <- doesDirectoryExist dest
  unless exists $
         callProcess "git" ["clone", "--depth", "1", "--branch", version, repoURI, dest]

build :: FilePath -> String -> IO ()
build root version =
  mapM_ exec
        [ ("autoconf", [])
        , (dest </> "configure", ["--prefix", foldl1 combine [root, "ruby", version]])
        , ("make", ["-k", "-j4"])
        , ("make", ["install"])
        ]
    where
      dest :: FilePath
      dest = getDest root version

      exec :: (FilePath, [String]) -> IO ()
      exec (cmd, args) = do
        (_, _, _, ph) <- createProcess (proc cmd args){ cwd = Just dest }
        code <- waitForProcess ph
        when (code /= ExitSuccess)
             exitFailure

ignoreNotExist :: IOError -> IO ()
ignoreNotExist = unless . isDoesNotExistError <*> throw

uninstall :: Command
uninstall _ [] = failWith "usage: monumental-ruby uninstall versions..."
uninstall root versions =
  mapM_ remove [root </> dir </> v | v <- versions, dir <- ["repo", "ruby"]]
    where
      remove :: FilePath -> IO ()
      remove = flip catch ignoreNotExist . removeDirectoryRecursive

use :: Command
use _ [] = failWith "use: 1 argument required"
use root [version] = do
  exists <- doesDirectoryExist src
  unless exists $
         failWith $ "use: not installed: " ++ show version
  removeDirectoryLink dest `catch` ignoreNotExist
  createSymbolicLink src dest
    where
      src :: FilePath
      src = foldl1 combine [root, "ruby", version, "bin"]

      dest :: FilePath
      dest = root </> "bin"
use _ _ = failWith "use: too many arguments"

list :: Command
list root [] = flip catch ignoreNotExist $ do
  dirs <- listDirectory $ root </> "ruby"
  mapM_ putStrLn dirs
list _ _ = failWith "usage: list"
