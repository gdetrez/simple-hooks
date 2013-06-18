module SimpleHooks.YamlSpec where

import SimpleHooks.Yaml -- SUT

import SimpleHooks.Data (Config(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "yaml config parser" $ do

    it "Accepts an simple configuration" $
      let conf = unlines  [ "pre-commit:"
                          , "  - test1"
                          , "  - make test2" ] in
      parseConfig conf `shouldBe` Right (Config [["test1"], ["make", "test2"]])

