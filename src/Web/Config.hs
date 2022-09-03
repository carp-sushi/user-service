module Web.Config
  ( Config(..)
  , loadConfig
  ) where

import Data.Configurator
import Data.Text

-- Service config type
data Config = Config
  { database    :: Text
  , port        :: Int
  , connections :: Int
  }
  deriving (Eq, Ord, Show)

-- Read service config from file path.
loadConfig :: FilePath -> IO Config
loadConfig file = do
  cfg <- load [Required file]
  db <- require cfg "db"
  port' <- require cfg "port"
  conn <- require cfg "connections"
  return $ Config db port' conn

