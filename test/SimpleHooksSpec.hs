{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module SimpleHooksSpec where

import SimpleHooks -- SUT

import Test.Hspec
import Shelly
import qualified Data.Text.Lazy as LT
import Prelude hiding (init, FilePath)
default (LT.Text)

-- shouldReturn :: (Eq a, Show a) => Sh a -> a -> Sh ()
-- shouldReturn ca expected = do
--   actual <- ca
--   liftIO $ shouldBe actual expected

spec :: Spec
spec = do
  describe "install" $ do

    it "throws an exception on a path that does not exists" $
      shelly (install "/tmp/i-most-probably-do-not-exists" False)
        `shouldThrow` anyException

    it "creates a pre-commit script" $ example $
      shelly (withTmpGitDir $ \dir ->
        install dir False >> test_f (dir </> ".git/hooks/pre-commit"))
          `shouldReturn` True

    it "creates a pre-commit script which is execitable" $ example $
      shelly $ withTmpGitDir $ \dir -> do
        install dir False
        cmd "test" "-x" (dir </> ".git/hooks/pre-commit")

    it "doesn't override an existing hook" $ example $
      shelly (withTmpGitDir $ \dir -> do
        writefile (dir </> ".git/hooks/pre-commit") "stuff"
        install dir False
        readfile (dir </> ".git/hooks/pre-commit")) `shouldReturn` "stuff"

    it "does override an existing hook if `force` is True" $
      shelly (withTmpGitDir $ \dir -> do
        writefile (dir </> ".git/hooks/pre-commit") "stuff"
        install dir True
        readfile (dir </> ".git/hooks/pre-commit")) `shouldReturn` preCommitScript

    it "creates a file .simple-hooks.yml" $ example $
      shelly $ withTmpGitDir $ \dir -> do
          install dir False
          cmd "test" "-f" (dir </> ".simple-hooks.yml")

    it "unless there is one in the repository already" $
      shelly (withTmpGitDir $ \dir -> do
         writefile (dir </> ".simple-hooks.yml") "stuff"
         install dir True
         readfile (dir </> ".simple-hooks.yml")) `shouldReturn` "stuff"



  describe "pre-commit" $ do

    it "creates a copy of the working dir" $ example $
      shelly (withTmpGitDir $ \dir -> do
        cd dir
        writefile "a.txt" "..."
        cmd "git" "add" "a.txt"
        preCommit dir ["test -f a.txt"]) `shouldReturn` [True]

    it "doesn't add untracked files" $ example $
      shelly (withTmpGitDir $ \dir -> do
        cd dir
        writefile "a.txt" "..."
        writefile "b.txt" "..."
        cmd "git" "add" "a.txt"
        preCommit dir ["! test -f b.txt"]) `shouldReturn` [True]

    it "runs all commands from the list" $ example $
      shelly (withTmpGitDir $ \dir -> do
        cd dir
        writefile "a.txt" "..."
        cmd "git" "add" "a.txt"
        preCommit dir ["echo abc", "test -f b.txt"])
      `shouldReturn` [True, False]

  describe "isGitDir" $ do

    it "returns false if the path does not exists" $
      shelly (isGitDir "/tmp/i-most-probably-do-not-exists") `shouldReturn` False

    it "return false if the path is a file" $
      shelly (withTmpDir $ \dir ->
        let f = (dir </> "somefile") in touchfile f >> isGitDir f)
          `shouldReturn` False

    it "returns false on an empty dir" $
      shelly (withTmpDir isGitDir) `shouldReturn` False

    it "returns true on a new .git dir" $
      shelly (withTmpGitDir $ \d -> isGitDir (d</>".git"))
        `shouldReturn` True

-- creates a temporary git directory:
withTmpGitDir :: (FilePath -> Sh a) -> Sh a
withTmpGitDir script = silently $
  withTmpDir $ \d -> cmd "git" "init" d >> script d
