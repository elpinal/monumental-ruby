module LibSpec (spec) where

import Test.Hspec
import Lib

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Either
import qualified Data.Map.Lazy as Map
import Data.Maybe

newtype TestIO a = TestIO (Reader FileMap a)

runTestIO :: TestIO a -> Reader FileMap a
runTestIO (TestIO x) = x

instance Functor TestIO where
  fmap f (TestIO x) = TestIO $ fmap f x

instance Applicative TestIO where
  pure = TestIO . reader . const
  (TestIO f) <*> (TestIO x) = TestIO $ f <*> x

instance Monad TestIO where
  (TestIO x) >>= f = TestIO $ x >>= runTestIO . f

instance MonadFS TestIO where
  readSym p = MaybeT . TestIO $ Map.lookup p . symMap <$> ask
  listDir p = MaybeT . TestIO $ Map.lookup p . dirMap <$> ask

data FileMap = FileMap
  { symMap :: Map.Map FilePath FilePath
  , dirMap :: Map.Map FilePath [FilePath]
  }

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
      runReader (runTestIO . runMaybeT $ getActive "root") m `shouldBe` Just "v2_3_4"

  describe "list'" $ do
    it "lists installed and active versions" $ do
      let m = FileMap { symMap = Map.singleton "root/bin" "root/ruby/v2_3_4/bin"
                      , dirMap = Map.singleton "root/ruby" ["v2_3_4"]
                      }
      runReader (runTestIO (list' "root")) m `shouldBe` [headerForInstalled, "v2_3_4", "", headerForActive, "v2_3_4", ""]
