{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module E01_Bare
  where

import           ClassyPrelude
import qualified Crypto.BCrypt          as BCrypt
import qualified Data.Aeson             as Aeson
import qualified Database.SQLite.Simple as Sql
import           Text.RawString.QQ

import           Common

-- hash the user's password
hashPassword :: User -> IO (Either AppError User)
hashPassword user = do
  mSalt <- BCrypt.genSaltUsingPolicy BCrypt.fastBcryptHashingPolicy
  let mHashPwd = mSalt >>= BCrypt.hashPassword (encodeUtf8 $ userPassword user)
  return $ case mHashPwd of
    Nothing -> Left FailedToHashPassword
    Just hp -> Right (user { userPassword = decodeUtf8 hp })

-- convert JSON ByteString to a User record
jsonToUser :: ByteString -> Either AppError User
jsonToUser rawJson =
  case Aeson.eitherDecodeStrict' rawJson of
    Left _  -> Left FailedToParseJSON
    Right u -> Right u

------------------------------------------------------------------------
-- Database
------------------------------------------------------------------------

createUsersTable :: Sql.Query
createUsersTable = [r|
  create table if not exists users
  ( id              integer primary key autoincrement
  , email           text not null unique
  , hashed_password text not null
  , name            text not null
  )
|]

-- get a user record from the DB given the email
getUserByEmail :: Sql.Connection -> Text -> IO (Either AppError User)
getUserByEmail conn email = do
  result <- Sql.queryNamed conn sql [":email" Sql.:= email]
  return $ case result of
    []     -> Left UserNotFound
    user:_ -> Right user
  where
    sql = [r| select id, email, hashed_password, name
              from users
              where email = :email
            |]

-- add a user to the database
addUser :: Sql.Connection -> User -> IO (Either AppError User)
addUser conn user = do
  Sql.executeNamed conn sql [ ":email" Sql.:= userEmail user
                            , ":hashed_password" Sql.:= userPassword user
                            , ":name" Sql.:= userName user
                            ]
  id' <- fromIntegral <$> Sql.lastInsertRowId conn
  return (Right user { userId = Just id' })
  where
    sql = [r| insert into users
              (email, hashed_password, name)
              values
              (:email, :hashed_password, :name)
            |]
createNewUser :: Sql.Connection -> User -> IO (Either AppError User)
createNewUser conn user = do
  user' <- getUserByEmail conn (userEmail user)
  case user' of
    Right _ -> return (Left UserAlreadyExists)
    Left  _ -> do
      user'' <- hashPassword user
      case user'' of
        Left e           -> return (Left e)
        Right hashedUser -> addUser conn hashedUser

{-

{"userEmail": "jonathan@curran.in", "userPassword": "123456", "userName": "joncfoo"}

-}

main :: IO ()
main = do
  config <- getAppConfig
  conn   <- Sql.open (appConfigSqliteFile config)
  _      <- Sql.execute_ conn createUsersTable

  line   <- getLine
  case jsonToUser line of
    Left err   -> putStr "Oops: " >> print err
    Right user -> do
      eUser <- createNewUser conn user
      case eUser of
        Left err -> putStr "Oops:"     >> print err
        Right u  -> putStr "Success: " >> print u
  Sql.close conn
