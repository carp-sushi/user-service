module Main (main) where

import Web.Config  (loadConfig)
import Web.Service (runService)

main :: IO ()
main = do
  cfg <- loadConfig "user-service.cfg"
  runService cfg

