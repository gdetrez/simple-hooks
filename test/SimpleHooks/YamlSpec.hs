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

