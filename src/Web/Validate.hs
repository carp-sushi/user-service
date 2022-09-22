module Web.Validate (
  Error (..),
  errorMessage,
  validateFirstName,
  validateLastName,
  validateEmail,
  validatePhone,
  validateTwitter,
  validateUser,
) where

import Data.Char (isAlphaNum, isNumber, isSpace)
import Data.Text as T
import Data.Validation
import Web.Models

newtype Error = Error [Text]
  deriving (Eq, Show)

instance Semigroup Error where
  Error e1 <> Error e2 = Error $ e1 <> e2

-- Convert an error back to text
errorMessage :: Error -> Text
errorMessage (Error xs) = T.unlines xs

-- Error helper
mkError :: Text -> Error
mkError e = Error [e]

-- Ensure text length does not exceed a given maximum
maxLength :: Int -> Text -> Validation Error Text
maxLength n t =
  if T.length t <= n
    then Success t
    else
      Failure $
        mkError $
          "cannot be longer than " <> T.pack (show n) <> " chars"

-- Ensure text has a required minimum length
minLength :: Int -> Text -> Validation Error Text
minLength n t =
  if T.length t >= n
    then Success t
    else
      Failure $
        mkError $
          "cannot be shorter than " <> T.pack (show n) <> " chars"

-- Ensure text has zero spaces
noSpaces :: Text -> Validation Error Text
noSpaces t =
  if T.any isSpace t
    then Failure $ mkError "cannot contain spaces"
    else Success t

-- Ensure text contains a single occurrence of another text
limitOne :: Text -> Text -> Validation Error Text
limitOne c t =
  if T.count c t == 1
    then Success t
    else Failure $ mkError $ "requires exactly one " <> c <> " char"

-- Ensure text has a given prefix
hasPrefix :: Text -> Text -> Validation Error Text
hasPrefix c t =
  if T.isPrefixOf c t
    then Success t
    else Failure $ mkError $ "requires prefix " <> c

-- Ensure text contains only numbers or the given char
numberOr :: Char -> Text -> Validation Error Text
numberOr c t =
  if T.all isValidChar t
    then Success t
    else Failure $ mkError $ "requires numbers or " <> T.pack [c]
  where
    isValidChar cc = cc == c || isNumber cc

-- Ensure text contains only alpha-numerics or the given char
alphaNumOr :: Char -> Text -> Validation Error Text
alphaNumOr c t =
  if T.all isValidChar t
    then Success t
    else Failure $ mkError $ "requires alpha-numerics or " <> T.pack [c]
  where
    isValidChar cc = cc == c || isAlphaNum cc

-- Email validation
validateEmail :: Text -> Validation Error Text
validateEmail email =
  case runValidation (T.strip email) of
    Failure e -> Failure $ mkError "Invalid email address:" <> e
    Success t -> Success t
  where
    runValidation t =
      limitOne "@" t
        *> noSpaces t
        *> minLength 3 t
        *> maxLength 320 t

-- First name validation
validateFirstName :: Text -> Validation Error Text
validateFirstName name =
  case runValidation (T.strip name) of
    Failure e -> Failure $ mkError "Invalid first name:" <> e
    Success t -> Success t
  where
    runValidation t =
      minLength 1 t
        *> maxLength 100 t

-- Last name validation
validateLastName :: Text -> Validation Error Text
validateLastName name =
  case runValidation (T.strip name) of
    Failure e -> Failure $ mkError "Invalid last name:" <> e
    Success t -> Success t
  where
    runValidation t =
      minLength 1 t
        *> maxLength 100 t

-- Phone number validation
validatePhone :: Text -> Validation Error Text
validatePhone phone =
  case runValidation (T.strip phone) of
    Failure e -> Failure $ mkError "Invalid phone number:" <> e
    Success t -> Success t
  where
    runValidation t =
      minLength 8 t
        *> maxLength 12 t
        *> numberOr '-' t

-- Twitter handle validation
validateTwitter :: Text -> Validation Error Text
validateTwitter handle =
  case runValidation (T.strip handle) of
    Failure e -> Failure $ mkError "Invalid twitter handle:" <> e
    Success t -> Success t
  where
    alphaNumOr_ = alphaNumOr '_'
    runValidation t =
      alphaNumOr_ (T.drop 1 t)
        *> minLength 2 t
        *> maxLength 100 t
        *> hasPrefix "@" t

-- User validation
validateUser :: User -> Validation Error User
validateUser user =
  User
    <$> validateFirstName (userFirstName user)
    <*> validateLastName (userLastName user)
    <*> validateEmail (userEmail user)
    <*> validatePhone (userPhoneNumber user)
    <*> validateTwitter (userTwitter user)
