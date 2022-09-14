module Main where

import Data.Validation
import Network.Wai
import Web.Spock       (spockAsApp)

import qualified Data.Text as T

import Web.Service
import Web.Validate

import Test.Hspec.Wai
import Test.Tasty
import Test.Tasty.Hspec

-- A too long chunk of text
longText :: T.Text
longText =
  "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvz"

-- Create a too long email address
longEmail :: T.Text
longEmail = t <> "@" <> t <> t <> "." <> t where t = longText

app :: IO Middleware
app = service "data/test.db" 5

spec_routes :: Spec
spec_routes =
  with (spockAsApp app) $ do
    describe "GET /status" $ do
      it "serves status" $
        get "/status" `shouldRespondWith` "{\"status\":\"ok\"}" {matchStatus = 200}

-- Test validation of first name
spec_validate_first :: Spec
spec_validate_first =
  describe "validate first name" $ do
    it "should fail when too short" $
      validateFirstName "" `shouldBe` Failure (Error ["Invalid first name:","cannot be shorter than 1 chars"])
    it "should fail when too long" $
      validateFirstName longText `shouldBe` Failure (Error ["Invalid first name:","cannot be longer than 100 chars"])

-- Test validation of last name
spec_validate_last :: Spec
spec_validate_last =
  describe "validate last name" $ do
    it "should fail when too short" $
      validateLastName "" `shouldBe` Failure (Error ["Invalid last name:","cannot be shorter than 1 chars"])
    it "should fail when too long" $
      validateLastName longText `shouldBe` Failure (Error ["Invalid last name:","cannot be longer than 100 chars"])

-- Test validation of email
spec_validate_email :: Spec
spec_validate_email =
  describe "validate email address" $ do
    it "should fail when too short" $
      validateEmail "a@" `shouldBe` Failure (Error ["Invalid email address:","cannot be shorter than 3 chars"])
    it "should fail when too long" $
      validateEmail longEmail `shouldBe` Failure (Error ["Invalid email address:","cannot be longer than 320 chars"])
    it "should fail when missing @ char" $
      validateEmail "abc" `shouldBe` Failure (Error ["Invalid email address:","requires exactly one @ char"])
    it "should fail when multiple @ chars found" $
      validateEmail "a@b@c.com" `shouldBe` Failure (Error ["Invalid email address:","requires exactly one @ char"])
    it "should fail when space chars found" $
      validateEmail "a@b c.com" `shouldBe` Failure (Error ["Invalid email address:","cannot contain spaces"])

-- Test validation of phone number
spec_validate_phone :: Spec
spec_validate_phone =
  describe "validate phone number" $ do
    it "should fail on invalid chars" $
      validatePhone "000)555_1234" `shouldBe` Failure (Error ["Invalid phone number:","requires numbers or -"])
    it "should fail when too short" $
      validatePhone "55-1234" `shouldBe` Failure (Error ["Invalid phone number:","cannot be shorter than 8 chars"])
    it "should fail when too long" $
      validatePhone "0000-555-1234" `shouldBe` Failure (Error ["Invalid phone number:","cannot be longer than 12 chars"])


-- Test validation of twitter text
spec_validate_twitter :: Spec
spec_validate_twitter =
  describe "validate twitter handle" $ do
    it "should fail on non alpha-numeric chars" $
      validateTwitter "@us$r@1" `shouldBe` Failure (Error ["Invalid twitter handle:","requires alpha-numerics or _"])
    it "should fail with missing @ prefix" $
      validateTwitter "user" `shouldBe` Failure (Error ["Invalid twitter handle:","requires prefix @"])
    it "should fail when too short" $
      validateTwitter "@" `shouldBe` Failure (Error ["Invalid twitter handle:","cannot be shorter than 2 chars"])
    it "should fail when too long" $
      validateTwitter ("@"<>longText) `shouldBe` Failure (Error ["Invalid twitter handle:","cannot be longer than 100 chars"])

-- Collect all specs
allSpecs :: [Spec]
allSpecs =
  [ spec_routes
  , spec_validate_first
  , spec_validate_last
  , spec_validate_email
  , spec_validate_phone
  , spec_validate_twitter
  ]

-- Run tests
main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs allSpecs
  defaultMain (testGroup "Specs" specs)

