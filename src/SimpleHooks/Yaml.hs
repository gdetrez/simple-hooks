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

module SimpleHooks.Yaml where

import SimpleHooks.Data
import Data.Yaml
import Data.ByteString.Char8 (pack)
import Data.Map as Map
import Debug.Trace

parseConfig :: String -> Either String Config
parseConfig str =
  let x = decodeEither (pack str) :: Either String (Map String [String])
  in case trace (show x) x of
    Left err -> Left err
    Right m -> case Map.lookup "pre-commit" m of
      Nothing -> Right $ Config []
      Just l -> Right $ Config (Prelude.map words l)

