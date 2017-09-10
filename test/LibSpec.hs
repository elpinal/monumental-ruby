module LibSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  describe "getDest" $ do
    it "gets destination" $ do
      getDest "root-directory" "v2_3_4" `shouldBe` "root-directory/repo/v2_3_4"

  describe "rootPath" $ do
    it "gets root path from home path" $ do
      let home = "home"
      rootPath home `shouldBe` "home/.monumental-ruby"
