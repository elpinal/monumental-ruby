{-|
The functions for the executable of monumental-ruby.
-}
module Lib where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Files
import System.Process

-- | The default repository URI to install.
repoURI :: String
repoURI = "https://github.com/ruby/ruby"

-- | Joins a directory with @".monumental-ruby"@.
rootPath :: FilePath -> FilePath
rootPath = flip combine ".monumental-ruby"

-- | The main function for the executable.
run :: IO ()
run = do
  args <- getArgs
  home <- getHomeDirectory
  result <- runExceptT $ run' home args
  case result of
    Left msg -> failWith msg
    _ -> return ()

run' :: FilePath -> [String] -> ExceptT String IO ()
run' home xs = do
  let (flags, args) = runState (runExceptT parseFlag) xs
  fs <- ExceptT . return $ flags
  root <- mapExceptT setRoot $ getRoot fs
  liftIO $ helpOrCmd fs root args
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

    helpOrCmd :: [Flag] -> CmdFunc
    helpOrCmd fs | Help `elem` fs = help
    helpOrCmd _ = doCmd

    setRoot :: Maybe (Either String FilePath) -> IO (Either String FilePath)
    setRoot Nothing = return . Right $ rootPath home
    setRoot (Just e) = return e

-- | Prints an error message to @stderr@ and exits with a failure code.
failWith :: String -> IO a
failWith msg = hPutStrLn stderr msg >> exitFailure

-- | Represents a global flag.
data Flag =
    Help
  | Root FilePath
    deriving (Eq, Ord, Show)

parseFlag :: ExceptT String (State [String]) [Flag]
parseFlag = get >>= parse
  where
    parse :: [String] -> ExceptT String (State [String]) [Flag]
    parse ("-h":xs) = do
      put xs
      flags <- parseFlag
      return $ Help : flags
    parse ("-root":xs) = do
      when (null xs)
           (throwError "-root: need argument")
      put $ tail xs
      flags <- parseFlag
      return $ Root (head xs) : flags
    parse (flag@('-':_):_) = throwError $
                               "no such flag: " ++ show flag ++ "\n" ++
                               "Run 'monumental-ruby help' for usage."
    parse args = do
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

-- | Exits with printing an error message which indicates that the provided
-- command is not defined.
nocmd :: String -> CmdFunc
nocmd x _ _ = failWith $
  "monumental-ruby: no such command " ++ show x ++ "\n" ++
  "Run 'monumental-ruby help' for usage."

-- | The usage for the executable.
usage :: String
usage =
  unlines $
    [ "Monumental-ruby is a tool for managing various versions of Ruby."
    , ""
    , "Usage:"
    , ""
    , replicate indent ' ' ++ "monumental-ruby [flags] command [arguments]"
    , ""
    , "Commands:"
    , ""
    ]
    ++
    [replicate indent ' ' ++ name c ++ replicate (align (name c)) ' ' ++ desc c | c <- Map.elems cmds]
    ++
    [ ""
    , "Flags:"
    , ""
    ]
    ++
    [replicate indent ' ' ++ f ++ replicate (align f) ' ' ++ d | (f, d) <- fs]
  where
    indent :: Int
    indent = 8

    longestNameLen :: Int
    longestNameLen = maximum . map length $ Map.keys cmds

    minSpaces :: Int
    minSpaces = 1

    align :: String -> Int
    align s = minSpaces + longestNameLen - length s

    fs :: [(String, String)]
    fs = [ ("-h", "show this help")
         , ("-root", "set root directory")
         ]

help :: CmdFunc
help _ [] = putStrLn usage
help _ [topic] = maybe noTopic (putStrLn . cmdUsage) $ Map.lookup topic cmds
  where
    noTopic :: IO ()
    noTopic = failWith $ "unknown help topic " ++ show topic ++ ". Run 'monumental-ruby help'."
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

type Version = String

getDest :: FilePath -> Version -> FilePath
getDest root version = foldl1 combine [root, "repo", version]

clone :: FilePath -> Version -> IO ()
clone root version = do
  let dest = getDest root version
  exists <- doesDirectoryExist dest
  unless exists $
         callProcess "git" ["clone", "--depth", "1", "--branch", version, repoURI, dest]

build :: FilePath -> Version -> IO ()
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
  dirs <- listDir $ root </> "ruby"
  putStrLn . highlight $ unlines
    [ "installed versions"
    , "------------------"
    ]
  mapM_ putStrLn dirs
  a <- getActive root
  putStrLn $ unlines
    [""
    , highlight
      "active version\n\
      \--------------"
    , ""
    , a
    ]
list _ _ = failWith "usage: list"

class Monad m => MonadSym m where
  readSym :: FilePath -> m FilePath
  listDir :: FilePath -> m [FilePath]

instance MonadSym IO where
  readSym = readSymbolicLink
  listDir = listDirectory

getActive :: MonadSym m => FilePath -> m Version
getActive root = return . takeFileName . takeDirectory <=< readSym $ root </> "bin"

highlight :: String -> String
highlight xs = "\ESC[1m" ++ xs ++ "\ESC[0m"
