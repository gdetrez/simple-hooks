{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module SimpleHooks where

import Control.Monad (liftM)
import Shelly
import qualified Data.Text.Lazy as LT
import Prelude hiding (init, FilePath)

default (LT.Text)

main :: IO ()
main = undefined

preCommitScript :: LT.Text
preCommitScript = LT.unlines
  [ "#!/bin/sh"
  , ""
  , "simple-hooks pre-commit" ]


-- Install the pre-commit hooks and creates a template hook file
install :: FilePath -- ^ The root of the git repository
        -> Bool     -- ^ If true, ovewrite existing hooks
        -> Sh ()
install dir force = do
    isGit <- isGitDir gitdir
    unless isGit $ fail "Not a git repository"
    exists <- test_f hookFile
    if exists && not force
      then echo "There is already a pre-commit hook in this repository, use -f to ovewrite"
      else writefile hookFile preCommitScript
    cmd "chmod" "+x" hookFile
  where gitdir = dir </> ".git"
        hookFile = gitdir </> "hooks/pre-commit"

isGitDir :: FilePath -> Sh Bool
isGitDir d = liftM and $ sequence
  [ test_d d
  , test_f (d </> "HEAD")
  , test_d (d </> "branches")
  , test_d (d </> "hooks")
  , test_d (d </> "info")
  , test_d (d </> "objects")
  , test_d (d </> "refs")
  , test_d (d </> "refs/heads")
  , test_d (d </> "refs/tags") ]

