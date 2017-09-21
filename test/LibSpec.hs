module LibSpec (spec) where

import Test.Hspec
import Lib

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Either
import qualified Data.Map.Lazy as Map

newtype TestSym a = TestSym (Reader (Map.Map FilePath FilePath) a)

runTestSym :: TestSym a -> Reader (Map.Map FilePath FilePath) a
runTestSym (TestSym x) = x

instance Functor TestSym where
  fmap f (TestSym x) = TestSym $ fmap f x

instance Applicative TestSym where
  pure = TestSym . reader . const
  (TestSym f) <*> (TestSym x) = TestSym $ f <*> x

instance Monad TestSym where
  (TestSym x) >>= f = TestSym $ x >>= (runTestSym . f)

instance MonadSym TestSym where
  readSym p = TestSym $ do
    m <- ask
    return $ Map.findWithDefault (f m) p m
      where
        f :: Map.Map FilePath FilePath -> a
        f m = error $ "not found " ++ show p ++ " in " ++ show m

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
      runReader (runTestSym (getActive "root")) (Map.singleton "root/bin" "foo/bar/v2_3_4/baz") `shouldBe` "v2_3_4"
