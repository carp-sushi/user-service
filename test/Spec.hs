module Main where

import Network.Wai

import Test.Hspec
import Test.Hspec.Wai

import Web.Service (service)
import Web.Spock (spockAsApp)

main :: IO ()
main = hspec spec

app :: IO Middleware
app = service "data/test.db" 5

spec :: Spec
spec = with (spockAsApp app) $ do
  describe "GET /" $ do
    it "serves status" $
      get "/status" `shouldRespondWith` "{\"status\":\"ok\"}" {matchStatus = 200}
