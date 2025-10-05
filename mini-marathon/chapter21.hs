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
