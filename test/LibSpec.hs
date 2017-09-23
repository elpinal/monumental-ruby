module LibSpec (spec) where

import Test.Hspec
import Lib

import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Either
import qualified Data.Map.Lazy as Map
import Data.Maybe

newtype TestIO a = TestIO (State FileMap a)

runTestIO :: TestIO a -> State FileMap a
runTestIO (TestIO x) = x

instance Functor TestIO where
  fmap f (TestIO x) = TestIO $ fmap f x

instance Applicative TestIO where
  pure = TestIO . gets . const
  (TestIO f) <*> (TestIO x) = TestIO $ f <*> x

instance Monad TestIO where
  (TestIO x) >>= f = TestIO $ x >>= runTestIO . f

instance MonadFS TestIO where
  readSym p = MaybeT . TestIO $ Map.lookup p . symMap <$> get
  listDir p = MaybeT . TestIO $ Map.lookup p . dirMap <$> get
  createSym src dest = TestIO $ put . updateSymMap (Map.insert dest src) =<< get
  removeDirLink p = MaybeT . TestIO $ do
    exists <- Map.member p . symMap <$> get
    put . updateSymMap (Map.delete p) =<< get
    return $ guard exists
  doesDirExist p = TestIO $ Map.member p . dirMap <$> get

updateSymMap :: (Map.Map FilePath FilePath -> Map.Map FilePath FilePath) -> FileMap -> FileMap
updateSymMap f m = m { symMap = f $ symMap m }

data FileMap = FileMap
  { symMap :: Map.Map FilePath FilePath
  , dirMap :: Map.Map FilePath [FilePath]
  }
  deriving (Eq, Show)

spec :: Spec
spec = do
  describe "getDest" $ do
    it "gets destination" $ do
      getDest "root-directory" "v2_3_4" `shouldBe` "root-directory/repo/v2_3_4"

  describe "rootPath" $ do
    it "gets root path from home path" $ do
      let home = "home"
      rootPath home `shouldBe` "home/.monumental-ruby"

  describe "parseFlag" $ do
    it "parses flags in args" $ do
      let xs = ["-h", "-root", "aaa", "arg1", "arg2"]
      let (flags, args) = runState (runExceptT parseFlag) xs
      flags `shouldBe` Right [Help, Root "aaa"]
      args `shouldBe` ["arg1", "arg2"]

    it "returns an error when given flag does not exist" $ do
      let xs = ["-no-such-flag", "arg"]
      let (flags, args) = runState (runExceptT parseFlag) xs
      isLeft flags `shouldBe` True
      args `shouldBe` ["-no-such-flag", "arg"]

  describe "highlight" $ do
    it "highlights string" $ do
      highlight "string" `shouldBe` "\ESC[1mstring\ESC[0m"

  describe "getActive" $ do
    it "gets active version" $ do
      let m = FileMap { symMap = Map.singleton "root/bin" "foo/bar/v2_3_4/baz"
                      , dirMap = Map.empty
                      }
      evalState (runTestIO . runMaybeT $ getActive "root") m `shouldBe` Just "v2_3_4"

  describe "list'" $ do
    it "lists installed and active versions" $ do
      let m = FileMap { symMap = Map.singleton "root/bin" "root/ruby/v2_3_4/bin"
                      , dirMap = Map.singleton "root/ruby" ["v2_2_2", "v2_3_4", "v2_4_1"]
                      }
      let want = [ headerForInstalled
                 , "v2_2_2", "v2_3_4", "v2_4_1"
                 , ""
                 , headerForActive
                 , "v2_3_4"
                 , ""
                 ]
      evalState (runTestIO (list' "root")) m `shouldBe` want

  describe "use'" $ do
    it "sets an active version" $ do
      let m = FileMap { symMap = Map.singleton "root/bin" "root/ruby/v2_3_4/bin"
                      , dirMap = Map.fromList
                                   [ ("root/ruby", ["v2_2_2", "v2_3_4", "v2_4_1"])
                                   , ("root/ruby/v2_2_2/bin", ["ruby"])
                                   ]
                      }
      -- TODO: Separate these lines.
      evalState (runTestIO (use' "root" "v2_2_2" >> runMaybeT (getActive "root"))) m `shouldBe` Just "v2_2_2"
      evalState (runTestIO (use' "root" "v2_2_2" >> use' "root" "v2_2_2")) m `shouldBe` Right ()
      evalState (runTestIO (use' "root" "v2_2_2" >> use' "root" "no_version")) m `shouldBe` Left "use: not installed: \"no_version\""
