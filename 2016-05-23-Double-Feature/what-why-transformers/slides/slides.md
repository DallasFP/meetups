class: center, middle, inverse
# Monad Transformers

### Jonathan Curran

<jonathan@curran.in>

---
# Recap

---
# Monoid

```
class Monoid a where
  mempty  :: a
  -- ^ Identity of 'mappend'

  mappend :: a -> a -> a
  -- ^ An associative operation

  mconcat :: [a] -> a
  -- ^ Fold a list using the monoid.
```

Something that has an "empty" value and can be "appended" to.

---
# Functor

```
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  -- ^ Apply a function over f.

  (<$) :: a -> f b -> f a
  -- ^ Replace all locations in the input with the same value.
  
(<$>) = fmap
-- ^ Infix version of `fmap`
```

---
# Functor Examples

```
fmap (+ 1) (Just 5) = Just 6
fmap (+ 1) Nothing  = Nothing

(+ 1) <$> (Left 5)  = Left 5
(+ 1) <$> (Right 5) = Right 6
```

---
# Applicative

```
class Functor f => Applicative f where
  pure :: a -> f a
  -- ^ Lift a value into f.

  (<*>) :: f (a -> b) -> f a -> f b
  -- ^ Apply a function in f to a value in f.
```

---
# Applicative Examples

```
Just  5 == pure 5
Right 5 == pure 5

Just (+ 1) <*> pure 5  = Just 6

Right (+1) <*> Right 5 = Right 6
Right (+1) <*> Left 5  = Left 5
```

---
# Monad

```
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  -- ^ Sequentially compose two actions, passing any value produced
  --   by the first as an argument to the second.

  (>>) :: m a -> m b -> m b
  -- ^ Sequentially compose two actions, discarding any value produced
  --   by the first.

  return :: a -> m a
  -- ^ Lift a value into `m`.

  fail :: String -> m a
  -- ^ Please don't use this. Legacy. kthxbai.
```

---
# Monad Examples:

```
Nothing >> Just 1  = Nothing

Right 1 >> Left 2  = Left 2

putStrLn "Hi" >> putStrLn "there"

getLine  :: IO String
putStrLn :: String -> IO ()

(>>=) :: m  a      -> (a      -> m  b ) -> m b
         IO String -> (String -> IO ()) -> IO ()

main = getLine >>= putStrLn
```

---
# Problem Statement

Authentication "service".  We'll focus on creating a new user.

Requirements:
- read DB config info from environment
- accept json user data
- hash user password
- create user if doesn't already exist
- output success/fail

For now simply use console input/output

---
# Attempt to translate requirements to types


---
read DB config info from environment

```
data AppConfig = AppConfig { ... }

appConfig :: IO AppConfig
```

---

accept json user data

```
data User = User { ... }

jsonToUser :: ByteString -> Maybe User
-- ^ No error message
```

--

```
jsonToUser :: ByteString -> Either String User
-- ^ Error message to use? Will have to copy/paste in tests.
```

--

```
data AppError = FailedToParseJSON

jsonToUser :: ByteString -> Either AppError User
```

Typed error is better & we can easily/responsibly extend it.

---
hash user password

```
hashPassword :: User -> User
-- ^ ideally
```

--

```
hashPassword :: User -> IO (Maybe User)
-- ^ using the bcrypt library this is what we'll have to deal with
```

--

```
data AppError = FailedToParseJSON | FailedToHashPassword

hashPassword :: User -> IO (Either AppError User)
-- ^ better, we can extend AppError with a typed error message
```

---
2 parts to: create user if doesn't already exist

1. get user with email
2. create user if doesn't exist

```
data AppError
  = FailedToParseJSON | FailedToHashPassword 
  | UserNotFound | UserAlreadyExists

getUserByEmail :: Connection -> Text -> Either AppError User

addUser :: Connection -> User -> Either AppError User

-- something like...
createNewUser =
  user = getUserByEmail
  if not user
  then addUser
  else error
```

---
# To the code

---
# Summary

- not too bad for first shot

--

- notice lots of `case` statements
  - lots of manual pattern matching

--

- where did the promise of composition go?
  - weren't monads supposed to help somehow?

- lots of plumbing

---
# Imagine a larger application

- lot more moving parts

- more of
  - configuration
  - types other than `Either AppError a`
  - managed resources
      - DB connection pool
      - http client
      - smtp client
      
---
class: center
# Sound fun eh?

![Dont Do It](nope.gif)

---

### Wish-list:

- short circuit w/o tree of doom
- dependency injection?
- [typed] logging

---
class: center

# Transformers

![Voltron](voltron.gif)

---

# What?

A monad transformer makes a new monad out of an existing monad, such that
computations of the old monad may be embedded in the new one.

--

> __You can stack monads__

--

To construct a monad with a desired set of features, one typically starts with a
base monad, such as Identity, [] or IO, and applies a sequence of monad
transformers.

--

> __There needs to be a base monad__

--

Each monad transformer also comes with an operation runXXX to unwrap the
transformer, exposing a computation of the inner monad.

--

> __You can get stuff out__

---
# Let's see some code

transformers package

---
# ReaderT Transformer

```
newtype ReaderT r (m :: * -> *) a
  = ReaderT {runReaderT :: r -> m a}
```

--

```
import Control.Monad.Trans.Reader (ReaderT, ask)

readStuff :: ReaderT AppConfig IO String
readStuff = do
  config <- ask
  return (appConfigSqliteFile config)
  
main = do
  config    <- getAppConfig
  readerOut <- runReaderT readStuff config
  print readerOut
```

---
# WriterT Transformer

```
newtype WriterT w (m :: * -> *) a
  = WriterT {runWriterT :: m (a, w)}
```

--

```
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

data Operations = This | That | Other | Another
  deriving (Show, Eq)

writeStuff :: WriterT [Operations] IO Int
writeStuff = do
  tell [This, Other]
  return 42

main = do
  writerOut <- runWriterT writeStuff
  print writerOut
```

---
# ExceptT Transformer

```
newtype ExceptT e (m :: * -> *) a 
  = ExceptT (m (Either e a))
```

--

```
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

throwStuff :: ExceptT AppError IO Int
throwStuff = do
  throwE FailedToHashPassword
  return 42

main = do
  throwOut <- runExceptT throwStuff
  print throwOut
```

---
# ReaderT, WriterT, & ExceptT ?

--

> Yes we can

```
readWriteThrow 
  :: ReaderT AppConfig (WriterT [Operations] (ExceptT AppError IO)) ()
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

main = do
  config <- getAppConfig
  rwtOut <- runExceptT (runWriterT (runReaderT readWriteThrow config))
  print rwtOut
 
```

---
# Nicer* RWT

```
type MyApp 
  = ReaderT AppConfig (WriterT [Operations] (ExceptT AppError IO))

nicerRWT :: MyApp Int
nicerRWT = do
  config <- ask
  lift.lift.lift $ print "nyaa"
  return 42

main = do
  config <- getAppConfig
  rwtOut <- runExceptT (runWriterT (runReaderT nicerRWT config))
  print rwtOut
```

\* for _some_ interpretations of _Nice_

---
# Hooray!?

`transformers` library is pretty sweet, however:

- notice all those `lift`s?
- types are gnarly
  - type errors even more so
  
---
# Enter MTL

mtl - monad transformer library

Monad classes using functional dependencies, with instances for various monad
transformers.

--

_It is a library of interfaces you can provide to your own types, in the form of
typeclasses._

&mdash; Justin Le

---
# What does this mean for us?

We can simply write:

```

readWriteThrow :: MyApp2
readWriteThrow = do
  config <- ask
  tell [This]
  if appConfigSqliteFile config == "test.db"
    then do
      tell [That]
      throwError UserAlreadyExists
    else do
      tell [Other]
      liftIO $ print "other thing happened"
      return ()
```

- No more lifts
- only `liftIO`

---
# With a bit of upfront work

```
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    , MonadError  AppError
    , MonadIO
    )

runMyApp2 :: AppConfig -> MyApp2 a -> IO (Either AppError (a, [Operations]))
runMyApp2 config app = runExceptT (runWriterT (runReaderT (unApp2 app) config))

main = do
  config <- getAppConfig
  out    <- runMyApp2 config readWriteThrow
  print out
```

---
## No really, what does this mean?

Since our application is now a Monad we can:

- have a true application context via the Reader monad
  - kind of like DI but way better

- avoid wrapping/unwrapping just so we can pattern matching everywhere

- terminate computations early in a sane manner

--

- chain computations

e.g.

```
jsonToUser    :: ByteString -> MyApp2 User
createNewUser :: User       -> MyApp2 User

-- in one shot
jsonToUser rawJson >>= createNewUser
```

---
class: center, middle

![Success](amazing.gif)

### Success!
