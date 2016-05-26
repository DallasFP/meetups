{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module E03_Mtl
  where

import           Common
import           Control.Monad.Except   (ExceptT, MonadError, runExceptT,
                                         throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.Writer   (MonadWriter, WriterT, runWriterT, tell)

newtype MyApp2 a =
  MyApp2
  {
    unApp2 :: ReaderT AppConfig (WriterT [Operations] (ExceptT AppError IO)) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader AppConfig
    , MonadWriter [Operations]
    , MonadError AppError
    , MonadIO
    )

runMyApp2 :: AppConfig -> MyApp2 a -> IO (Either AppError (a, [Operations]))
runMyApp2 config app = runExceptT (runWriterT (runReaderT (unApp2 app) config))

data Operations = This | That | Other | Another
  deriving (Show, Eq)

readWriteThrow :: MyApp2 ()
readWriteThrow = do
  config <- ask
  tell [This]
  if appConfigSqliteFile config == "test.db"
    then do
      tell [That]
      throwError UserAlreadyExists
    else do
      tell [Other]
      return ()

main :: IO ()
main = do
  config <- getAppConfig
  out    <- runMyApp2 config readWriteThrow
  print out
