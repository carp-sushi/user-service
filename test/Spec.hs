import Test.Hspec.Wai
import Test.Tasty
import Test.Tasty.Hspec

import qualified Data.Text as T
import Data.Validation
import Network.Wai
import Web.Spock (spockAsApp)

import Web.Service
import Web.Validate

-- A too long chunk of text
longText :: T.Text
longText =
  "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxy"
    <> "zabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvzz"

-- Create a too long email address
longEmail :: T.Text
longEmail = t <> "@" <> t <> t <> "." <> t where t = longText

-- Failure helper
mkFailure :: T.Text -> T.Text -> Validation Error T.Text
mkFailure x y = Failure $ Error [x, y]

-- Test middleware
app :: IO Middleware
app = service "data/test.db" 1

-- Test http routes
spec_routes :: Spec
spec_routes =
  with (spockAsApp app) $ do
    describe "GET /status" $ do
      it "serves status" $
        get "/status" `shouldRespondWith` "{\"status\":\"ok\"}" {matchStatus = 200}

-- Test validation of first name
spec_validate_first :: Spec
spec_validate_first = do
  let failure = mkFailure "Invalid first name:"
  describe "validate first name" $ do
    it "should trim leading/trailing spaces" $
      validateFirstName " Jane " `shouldBe` Success "Jane"
    it "should fail when too short" $
      validateFirstName "" `shouldBe` failure "cannot be shorter than 1 chars"
    it "should fail when too long" $
      validateFirstName longText `shouldBe` failure "cannot be longer than 100 chars"

-- Test validation of last name
spec_validate_last :: Spec
spec_validate_last = do
  let failure = mkFailure "Invalid last name:"
  describe "validate last name" $ do
    it "should trim leading/trailing spaces" $
      validateLastName " Smith " `shouldBe` Success "Smith"
    it "should fail when too short" $
      validateLastName "" `shouldBe` failure "cannot be shorter than 1 chars"
    it "should fail when too long" $
      validateLastName longText `shouldBe` failure "cannot be longer than 100 chars"

-- Test validation of email
spec_validate_email :: Spec
spec_validate_email = do
  let failure = mkFailure "Invalid email address:"
  describe "validate email address" $ do
    it "should trim leading/trailing spaces" $
      validateEmail " user@email.com " `shouldBe` Success "user@email.com"
    it "should fail when too short" $
      validateEmail "a@" `shouldBe` failure "cannot be shorter than 3 chars"
    it "should fail when too long" $
      validateEmail longEmail `shouldBe` failure "cannot be longer than 320 chars"
    it "should fail when missing @ char" $
      validateEmail "abc" `shouldBe` failure "requires exactly one @ char"
    it "should fail when multiple @ chars found" $
      validateEmail "a@b@c.com" `shouldBe` failure "requires exactly one @ char"
    it "should fail when space chars found" $
      validateEmail "a@b c.com" `shouldBe` failure "cannot contain spaces"

-- Test validation of phone number
spec_validate_phone :: Spec
spec_validate_phone = do
  let failure = mkFailure "Invalid phone number:"
  describe "validate phone number" $ do
    it "should trim leading/trailing spaces" $
      validatePhone " 000-555-1234 " `shouldBe` Success "000-555-1234"
    it "should fail on invalid chars" $
      validatePhone "000)555_1234" `shouldBe` failure "requires numbers or -"
    it "should fail when too short" $
      validatePhone "55-1234" `shouldBe` failure "cannot be shorter than 8 chars"
    it "should fail when too long" $
      validatePhone "0000-555-1234" `shouldBe` failure "cannot be longer than 12 chars"

-- Test validation of twitter text
spec_validate_twitter :: Spec
spec_validate_twitter = do
  let failure = mkFailure "Invalid twitter handle:"
  describe "validate twitter handle" $ do
    it "should trim leading/trailing spaces" $
      validateTwitter " @twitter " `shouldBe` Success "@twitter"
    it "should fail on non alpha-numeric chars" $
      validateTwitter "@us$r@1" `shouldBe` failure "requires alpha-numerics or _"
    it "should fail with missing @ prefix" $
      validateTwitter "user" `shouldBe` failure "requires prefix @"
    it "should fail when too short" $
      validateTwitter "@" `shouldBe` failure "cannot be shorter than 2 chars"
    it "should fail when too long" $
      validateTwitter ("@" <> longText) `shouldBe` failure "cannot be longer than 100 chars"

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
  defaultMain (testGroup "User Service Specs" specs)
