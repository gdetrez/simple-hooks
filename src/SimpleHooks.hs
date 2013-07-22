{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module SimpleHooks where

import Control.Monad (liftM,unless)
import Shelly
import qualified Data.Text.Lazy as LT
import Prelude hiding (init, FilePath)

default (LT.Text)

main :: IO ()
main = undefined

-- TOHO: Those should be included using TH
preCommitScript :: LT.Text
preCommitScript = LT.unlines
  [ "#!/bin/sh"
  , ""
  , "simple-hooks pre-commit" ]

exampleConfFile :: LT.Text
exampleConfFile = LT.unlines
  [ "pre-commit"
  , "  - make tests"
  , "  - jslint" ]


-- Install the pre-commit hooks and creates a template hook file
install :: FilePath -- ^ The root of the git repository
        -> Bool     -- ^ If true, ovewrite existing hooks
        -> Sh ()
install dir force = do
    isGit <- isGitDir gitdir
    unless isGit $ fail "Not a git repository"
    -- Hook script
    exists <- test_f hookFile
    if exists && not force
      then echo "There is already a pre-commit hook in this repository, use -f to ovewrite"
      else writefile hookFile preCommitScript
    cmd "chmod" "+x" hookFile
    -- conf file
    exists <- test_f confFile
    unless exists $ writefile confFile exampleConfFile
  where gitdir = dir </> ".git"
        hookFile = gitdir </> "hooks/pre-commit"
        confFile = dir </> ".simple-hooks.yml"


-- Run the pre-commit hooks
preCommit :: FilePath   -- ^ The root of the git repository
          -> [LT.Text] -- ^ Commands to run
          -> Sh ()
preCommit dir cmds = do
    -- withTmpDir $ \d ->
    run_ "sh" ["-c", head cmds]



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

