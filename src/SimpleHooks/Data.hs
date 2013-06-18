module SimpleHooks.Data where

data Config = Config Hooks
  deriving (Eq, Show)
type Hooks = [ [ String ] ]


