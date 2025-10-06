-- WriterDemo.hs

newtype Writer w a = Writer { runWriter :: (a, w) }

-- Functor instance
instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

-- Applicative instance
instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  Writer (f, w1) <*> Writer (a, w2) = Writer (f a, w1 <> w2)

-- Monad instance
instance Monoid w => Monad (Writer w) where
  return = pure
  Writer (a, w1) >>= f =
    let Writer (b, w2) = f a
    in Writer (b, w1 <> w2)

-- Tell function
tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

-- Arithmetic with logging
addW :: Int -> Int -> Writer [String] Int
addW x y = do
  let r = x + y
  tell ["Added " ++ show x ++ " and " ++ show y ++ " = " ++ show r]
  return r

subW :: Int -> Int -> Writer [String] Int
subW x y = do
  let r = x - y
  tell ["Subtracted " ++ show y ++ " from " ++ show x ++ " = " ++ show r]
  return r

mulW :: Int -> Int -> Writer [String] Int
mulW x y = do
  let r = x * y
  tell ["Multiplied " ++ show x ++ " and " ++ show y ++ " = " ++ show r]
  return r

-- Demo calculation
calcDemo :: Writer [String] Int
calcDemo = do
  a <- addW 3 5
  b <- subW a 2
  c <- mulW b 4
  return c

-- Main to run it
main :: IO ()
main = print (runWriter calcDemo)

-- WriterDemo.hs

-- Define Writer
newtype Writer w a = Writer { runWriter :: (a, w) }
  deriving Show

-- Functor instance
instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

-- Applicative instance (use mempty and <> as in the lesson)
instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  Writer (f, w1) <*> Writer (a, w2) =
    Writer (f a, w1 <> w2)

-- Monad instance (concatenate logs in order)
instance Monoid w => Monad (Writer w) where
  return = pure
  Writer (a, w1) >>= f =
    let Writer (b, w2) = f a
    in Writer (b, w1 <> w2)

-- Tell: log some output
tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

-- Arithmetic with logging
addW :: Int -> Int -> Writer [String] Int
addW x y = do
  let r = x + y
  tell ["Added " ++ show x ++ " and " ++ show y ++ " = " ++ show r]
  return r

subW :: Int -> Int -> Writer [String] Int
subW x y = do
  let r = x - y
  tell ["Subtracted " ++ show y ++ " from " ++ show x ++ " = " ++ show r]
  return r

mulW :: Int -> Int -> Writer [String] Int
mulW x y = do
  let r = x * y
  tell ["Multiplied " ++ show x ++ " and " ++ show y ++ " = " ++ show r]
  return r

-- Demo calculation
calcDemo :: Writer [String] Int
calcDemo = do
  a <- addW 3 5
  b <- subW a 2
  c <- mulW b 4
  return c

----------------------------------------
-- Quick manual law checks
----------------------------------------

-- Functor law: fmap id = id
checkFunctorId :: (Eq a, Eq w) => (a,w) -> Bool
checkFunctorId (x,w) =
  runWriter (fmap id (Writer (x,w))) == runWriter (Writer (x,w))

-- Applicative law: pure id <*> v = v
checkApplicativeId :: (Eq a, Eq w, Monoid w) => (a,w) -> Bool
checkApplicativeId (x,w) =
  runWriter (pure id <*> Writer (x,w)) == runWriter (Writer (x,w))

-- Monad law: associativity
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)
checkMonadAssoc :: (Eq a, Eq w, Monoid w)
                => Writer w a
                -> (a -> Writer w a)
                -> (a -> Writer w a)
                -> Bool
checkMonadAssoc m f g =
  runWriter ((m >>= f) >>= g) == runWriter (m >>= (\x -> f x >>= g))

----------------------------------------
-- Main
----------------------------------------
main :: IO ()
main = do
  putStrLn "=== Demo Calculation ==="
  print (runWriter calcDemo)

  putStrLn "\n=== Law Checks ==="
  print (checkFunctorId (42, ["log"]))
  print (checkApplicativeId (7, ["hi"]))

  let m = Writer (3, ["m"])
      f x = Writer (x + 1, ["f"])
      g x = Writer (x * 2, ["g"])
  print (checkMonadAssoc m f g)

-- WriterListenPass.hs

newtype Writer w a = Writer { runWriter :: (a, w) }
  deriving Show

-- Functor
instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

-- Applicative
instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  Writer (f, w1) <*> Writer (a, w2) = Writer (f a, w1 <> w2)

-- Monad
instance Monoid w => Monad (Writer w) where
  return = pure
  Writer (a, w1) >>= f =
    let Writer (b, w2) = f a
    in Writer (b, w1 <> w2)

-- tell: append log
tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

-- listen: expose current log
listen :: Monoid w => Writer w a -> Writer w (a, w)
listen (Writer (a, w)) = Writer ((a, w), w)

-- pass: post-process the log with a function
pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
pass (Writer ((a, f), w)) = Writer (a, f w)

---------------------------------------------------
-- Example arithmetic with possible "secret" logs
---------------------------------------------------

addW :: Int -> Int -> Writer [String] Int
addW x y = do
  let r = x + y
  tell ["Added " ++ show x ++ " and " ++ show y ++ " = " ++ show r]
  return r

secretW :: Int -> Writer [String] Int
secretW x = do
  tell ["secret: used hidden value " ++ show x]
  return x

---------------------------------------------------
-- Redacting logs with "secret"
---------------------------------------------------

-- Function to remove any log lines containing "secret"
redactSecrets :: [String] -> [String]
redactSecrets = filter (not . ("secret" `elem`) . words)

demo :: Writer [String] Int
demo = do
  a <- addW 2 3
  b <- secretW 42
  c <- addW a b
  return c

demoRedacted :: Writer [String] Int
demoRedacted = pass $ do
  x <- demo
  return (x, redactSecrets)

---------------------------------------------------
-- Main
---------------------------------------------------
main :: IO ()
main = do
  putStrLn "=== Normal log ==="
  print (runWriter demo)

  putStrLn "\n=== With secrets redacted ==="
  print (runWriter demoRedacted)

  putStrLn "\n=== Using listen to inspect mid-computation ==="
  print (runWriter (listen demo))

-- WriterCount.hs

import Data.Monoid (Sum(..), Product(..))

newtype Writer w a = Writer { runWriter :: (a, w) }
  deriving Show

-- Functor
instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

-- Applicative
instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  Writer (f, w1) <*> Writer (a, w2) = Writer (f a, w1 <> w2)

-- Monad
instance Monoid w => Monad (Writer w) where
  return = pure
  Writer (a, w1) >>= f =
    let Writer (b, w2) = f a
    in Writer (b, w1 <> w2)

-- tell: accumulate effect
tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

---------------------------------------------------
-- Arithmetic now just increments the count
---------------------------------------------------

addW :: Int -> Int -> Writer (Sum Int) Int
addW x y = do
  let r = x + y
  tell (Sum 1)   -- count one operation
  return r

subW :: Int -> Int -> Writer (Sum Int) Int
subW x y = do
  let r = x - y
  tell (Sum 1)
  return r

mulW :: Int -> Int -> Writer (Sum Int) Int
mulW x y = do
  let r = x * y
  tell (Sum 1)
  return r

calcDemo :: Writer (Sum Int) Int
calcDemo = do
  a <- addW 3 5
  b <- subW a 2
  c <- mulW b 4
  return c

---------------------------------------------------
-- Main
---------------------------------------------------
main :: IO ()
main = do
  putStrLn "=== Counting steps with Sum Int ==="
  print (runWriter calcDemo)

  putStrLn "\n=== Counting steps with Product Int ==="
  -- reuse same functions but change tell to Product if desired
  let demoProduct = do
        a <- Writer (3+5, Product 1)
        b <- Writer (a-2, Product 1)
        c <- Writer (b*4, Product 1)
        return c
  print (runWriter demoProduct)


import Data.Char (toUpper)

-- ReaderDemo.hs

newtype Reader r a = Reader { runReader :: r -> a }

-- Functor
instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (\r -> f (ra r))

-- Applicative
instance Applicative (Reader r) where
  pure a = Reader (\_ -> a)
  Reader rf <*> Reader ra = Reader (\r -> rf r (ra r))

-- Monad
instance Monad (Reader r) where
  return = pure
  Reader ra >>= f = Reader (\r -> runReader (f (ra r)) r)

-- Ask for the environment
ask :: Reader r r
ask = Reader id

-- Local: modify environment for a subcomputation
local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader ra) = Reader (\r -> ra (f r))

---------------------------------------------------
-- Example Config
---------------------------------------------------

data Config = Config
  { greetPrefix :: String
  , shout       :: Bool
  } deriving Show

-- Greeting function
greet :: String -> Reader Config String
greet name = do
  cfg <- ask
  let base = greetPrefix cfg ++ " " ++ name
      msg  = if shout cfg then map toUpper base else base
  -- Demonstrate using local to flip shout for a sub-call
  alt <- local (\c -> c { shout = not (shout c) }) (greetSimple name)
  return (msg ++ " | alt: " ++ alt)

-- A simpler greet without nesting (used inside local)
greetSimple :: String -> Reader Config String
greetSimple name = do
  cfg <- ask
  let base = greetPrefix cfg ++ " " ++ name
  return (if shout cfg then map toUpper base else base)

---------------------------------------------------
-- Main
---------------------------------------------------


main :: IO ()
main = do
  let cfg1 = Config "Hello" False
      cfg2 = Config "Hi" True
  putStrLn "=== Normal config (not shouting) ==="
  putStrLn (runReader (greet "Alice") cfg1)

  putStrLn "\n=== Shouting config ==="
  putStrLn (runReader (greet "Bob") cfg2)


-- ReaderLesson.hs

import Data.Char (toUpper)

-- Reader type
newtype Reader r a = Reader { runReader :: r -> a }

---------------------------------------------------
-- Instances (lesson style)
---------------------------------------------------

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  pure a = Reader (\_ -> a)
  Reader rf <*> Reader ra = Reader (\r -> rf r (ra r))

instance Monad (Reader r) where
  return = pure
  Reader ra >>= f = Reader (\r -> runReader (f (ra r)) r)

---------------------------------------------------
-- Ask / Local
---------------------------------------------------

ask :: Reader r r
ask = Reader id

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader ra) = Reader (\r -> ra (f r))

---------------------------------------------------
-- Example Config
---------------------------------------------------

data Config = Config
  { greetPrefix :: String
  , shout       :: Bool
  } deriving Show

-- First Reader action: greeting
greet :: String -> Reader Config String
greet name = do
  cfg <- ask
  let base = greetPrefix cfg ++ " " ++ name
  return (if shout cfg then map toUpper base else base)

-- Second Reader action: farewell
farewell :: String -> Reader Config String
farewell name = do
  cfg <- ask
  let base = "Bye " ++ name
  return (if shout cfg then map toUpper base else base)

-- Compose two Reader actions with shared environment
conversation :: String -> Reader Config String
conversation name = do
  g <- greet name
  f <- farewell name
  return (g ++ " ... " ++ f)

---------------------------------------------------
-- Main
---------------------------------------------------
main :: IO ()
main = do
  let cfg1 = Config "Hello" False
      cfg2 = Config "Hi" True

  putStrLn "=== Normal config ==="
  putStrLn (runReader (conversation "Alice") cfg1)

  putStrLn "\n=== Shouting config ==="
  putStrLn (runReader (conversation "Bob") cfg2)


-- ReaderEnv.hs

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)

-- Reader type
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  pure a = Reader (\_ -> a)
  Reader rf <*> Reader ra = Reader (\r -> rf r (ra r))

instance Monad (Reader r) where
  return = pure
  Reader ra >>= f = Reader (\r -> runReader (f (ra r)) r)

-- Ask / Local
ask :: Reader r r
ask = Reader id

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader ra) = Reader (\r -> ra (f r))

---------------------------------------------------
-- Environment-based config
---------------------------------------------------

type Env = Map String String

-- Functions explicitly taking Env
getDbHost :: Env -> String
getDbHost env = fromMaybe "localhost" (M.lookup "DB_HOST" env)

getDbPort :: Env -> String
getDbPort env = fromMaybe "5432" (M.lookup "DB_PORT" env)

-- Refactored into Reader
getDbHostR :: Reader Env String
getDbHostR = Reader getDbHost

getDbPortR :: Reader Env String
getDbPortR = Reader getDbPort

-- Combine them with Reader
connectionString :: Reader Env String
connectionString = do
  host <- getDbHostR
  port <- getDbPortR
  return ("postgresql://" ++ host ++ ":" ++ port)

-- Override one key locally
altConnectionString :: Reader Env String
altConnectionString =
  local (M.insert "DB_PORT" "9999") connectionString

---------------------------------------------------
-- Main
---------------------------------------------------
main :: IO ()
main = do
  let env = M.fromList [("DB_HOST","db.example.com"),("DB_PORT","5432")]

  putStrLn "=== Normal connection string ==="
  putStrLn (runReader connectionString env)

  putStrLn "\n=== With local override of DB_PORT ==="
  putStrLn (runReader altConnectionString env)

