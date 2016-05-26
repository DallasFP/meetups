{-# LANGUAGE OverloadedStrings #-}

module E02_Transformers
  where

import           Common
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

readStuff :: ReaderT AppConfig IO String
readStuff = do
  config <- ask
  return (appConfigSqliteFile config)

data Operations = This | That | Other | Another
  deriving (Show, Eq)

writeStuff :: WriterT [Operations] IO Int
writeStuff = do
  tell [This, Other]
  return 42

throwStuff :: ExceptT AppError IO Int
throwStuff = do
  throwE FailedToHashPassword
  return 42

readWriteThrow :: ReaderT AppConfig (WriterT [Operations] (ExceptT AppError IO)) ()
readWriteThrow = do
  config <- ask
  lift $ tell [This]
  if appConfigSqliteFile config == "test.db"
    then do
      lift $ tell [That]
      lift.lift $ throwE UserAlreadyExists
    else do
      lift $ tell [Other]
      return ()

main :: IO ()
main = do
  config <- getAppConfig
  readerOut <- runReaderT readStuff config
  print readerOut

  writerOut <- runWriterT writeStuff
  print writerOut

  throwOut <- runExceptT throwStuff
  print throwOut

  rwtOut <- runExceptT (runWriterT (runReaderT readWriteThrow config))
  print rwtOut
