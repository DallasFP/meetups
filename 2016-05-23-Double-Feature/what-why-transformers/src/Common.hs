{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Common
  where

import           ClassyPrelude
import           Data.Aeson.Types       (FromJSON, ToJSON)
import           Database.SQLite.Simple (FromRow (..), field)
import qualified Prelude
import           System.Environment     (getEnv)

-- Application specific parameters read from the environment
data AppConfig =
  AppConfig
  {
    appConfigSqliteFile :: !String
  , appConfigHttpPort   :: !Integer
  }
  deriving (Show)

-- reads configuration data from the environment
getAppConfig :: IO AppConfig
getAppConfig = do
  fpath <- getEnv "APP_SQLITE_FILE"
  port  <- Prelude.read <$> getEnv "APP_HTTP_PORT"
  return $ AppConfig fpath port

-- simple user data type
data User =
  User
  { userId       :: !(Maybe Integer)
  , userEmail    :: !Text
  , userPassword :: !Text
  , userName     :: !Text
  }
  deriving (Show, Generic)  -- notice the Generic type we are deriving

-- auto-generated JSON [un]/marshalling instance for our User record
instance FromJSON User
instance ToJSON User

-- Allow a query response to be converted into a User record
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

-- possible application specific errors
data AppError
  = FailedToHashPassword
  | FailedToParseJSON
  | UserAlreadyExists
  | UserNotFound
  deriving (Show, Eq)

