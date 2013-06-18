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

