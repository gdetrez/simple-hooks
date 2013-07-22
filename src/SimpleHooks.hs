{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- simple-hooks: an easier way to define git hooks
-- Copyright (C) 2013 Grégoire Détrez
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see [http://www.gnu.org/licenses/].

module SimpleHooks where

import Control.Monad (liftM)
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
          -> Sh [Bool]
preCommit dir cmds = withTmpDir $ \tmpdir -> do
    echo "Running pre-commit hooks now…"
    cmd "git" "checkout-index" "--prefix" (tmpdir </> "") "-af"
    cd tmpdir
    mapM run' cmds
  where run' cmd = errExit False $ do
          run_ "sh" ["-c", cmd]
          success <- lastExitCode >>= return . (== 0)
          if success
            then echo $ LT.unwords [ "[", pass, "]", cmd ]
            else echo $ LT.unwords [ "[", fail, "]", cmd ]
          return success
        fail = "\ESC[31mFAIL\ESC[0m"
        pass = "\ESC[32mPASS\ESC[0m"

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

