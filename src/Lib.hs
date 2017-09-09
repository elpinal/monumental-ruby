module Lib where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map
import Data.Maybe
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

rootPath :: FilePath -> FilePath
rootPath = flip combine ".monumental-ruby"

run :: IO ()
run = do
  args <- getArgs
  home <- getHomeDirectory
  run' home args

run' :: FilePath -> [String] -> IO ()
run' home xs = do
  let (flags, args) = runState (runExceptT parseFlag) xs
  case flags of
    Left msg -> failWith msg
    Right fs -> do
      let k = runExceptT (getRoot fs)
      root <- do
        if isNothing k then
          return $ rootPath home
        else
          case fromJust k of
            Left msg -> failWith msg
            Right p -> return $ rootPath p
      if Help `elem` fs then
        help root args
      else
        doCmd root args
  where
    getRoot :: [Flag] -> ExceptT String Maybe FilePath
    getRoot fs =
      case filter isRoot fs of
        [Root p] -> return p
        [] -> fail "no root specified"
        _ -> throwError "duplicated -root flags"
    isRoot :: Flag -> Bool
    isRoot (Root _) = True
    isRoot _ = False

failWith :: String -> IO a
failWith msg = hPutStrLn stderr msg >> exitFailure

data Flag =
    Help
  | Root FilePath
    deriving (Eq, Ord, Show)

parseFlag :: ExceptT String (State [String]) [Flag]
parseFlag = do
  args <- get
  case args of
    ("-h":xs) -> do
      put xs
      flags <- parseFlag
      return $ Help : flags
    ("-root":xs) -> do
      when (length xs == 0)
           (throwError "-root: need argument")
      put $ tail xs
      flags <- parseFlag
      return $ Root (head xs) : flags
    (('-':flag):_) -> throwError $ "no such flag: " ++ show flag
    _ -> do
      put args
      return []

type CmdFunc = FilePath -> [String] -> IO ()

data Command = Command { name :: String
                       , func :: CmdFunc
                       , cmdUsage :: String
                       , desc :: String
                       }

cmds :: Map.Map String Command
cmds =
  Map.fromList [ ("install",
                  Command { name = "install"
                          , func = install
                          , cmdUsage = "usage: monumental-ruby install versions..."
                          , desc = "install specified versions of Ruby"
                          })
               , ("uninstall",
                  Command { name = "uninstall"
                          , func = uninstall
                          , cmdUsage = "usage: monumental-ruby uninstall versions..."
                          , desc = "uninstall specified versions of Ruby"
                          })
               , ("use",
                  Command { name = "use"
                          , func = use
                          , cmdUsage = "usage: monumental-ruby use version"
                          , desc = "select the specific version of Ruby as cureent version"
                          })
               , ("list",
                  Command { name = "list"
                          , func = list
                          , cmdUsage = "usage: monumental-ruby list"
                          , desc = "list installed versions of Ruby"
                          })
               , ("help",
                  Command { name = "help"
                          , func = help
                          , cmdUsage = "usage: monumental-ruby help [topic]"
                          , desc = "show help"
                          })
               ]

doCmd :: CmdFunc
doCmd _ [] = putStrLn usage >> exitFailure
doCmd root (name:args) = cmd name root args
  where
    cmd :: String -> CmdFunc
    cmd x = maybe (nocmd x) func $ Map.lookup x cmds

nocmd :: String -> CmdFunc
nocmd x _ _ = failWith $ "monumental-ruby: no such command " ++ show x

usage :: String
usage =
  unlines $
    [ "Monumental-ruby is a tool for managing various versions of Ruby."
    , ""
    , "Usage:"
    , ""
    ]
    ++
    [replicate indent ' ' ++ name c ++ replicate (align c) ' ' ++ desc c | c <- Map.elems cmds]
  where
    indent :: Int
    indent = 8

    longestNameLen :: Int
    longestNameLen = maximum . map length $ Map.keys cmds

    minSpaces :: Int
    minSpaces = 4

    align :: Command -> Int
    align Command {name = n} = minSpaces + longestNameLen - length n

help :: CmdFunc
help _ [] = putStrLn usage
help _ [topic] = helpOf topic
  where
    helpOf :: String -> IO ()
    helpOf topic = maybe (noTopic topic) (putStrLn . cmdUsage) $ Map.lookup topic cmds

    noTopic :: String -> IO ()
    noTopic topic = failWith $ "unknown help topic " ++ show topic ++ ". Run 'monumental-ruby help'."
help _ _ =
  failWith $
    unlines [ "usage: monumental-ruby help command"
            , ""
            , "Too many arguments given."
            ]

install :: CmdFunc
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

uninstall :: CmdFunc
uninstall _ [] = failWith "usage: monumental-ruby uninstall versions..."
uninstall root versions =
  mapM_ remove [root </> dir </> v | v <- versions, dir <- ["repo", "ruby"]]
    where
      remove :: FilePath -> IO ()
      remove = flip catch ignoreNotExist . removeDirectoryRecursive

use :: CmdFunc
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

list :: CmdFunc
list root [] = flip catch ignoreNotExist $ do
  dirs <- listDirectory $ root </> "ruby"
  mapM_ putStrLn dirs
list _ _ = failWith "usage: list"
