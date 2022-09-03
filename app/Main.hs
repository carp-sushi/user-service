module Main (main) where

import Web.Config  (loadConfig)
import Web.Service (runService)

main :: IO ()
main = do
  loadConfig "user-service.cfg" >>= runService

