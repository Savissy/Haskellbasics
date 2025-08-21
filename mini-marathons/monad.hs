

1. <$> (fmap) - The Functor Operator

What it is: Applies a pure function to a value inside a computational context (e.g., a `Maybe`, a `List`, an `IO` action). Pronounced "f-map".

Type Signature:
haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b

"Take a function from `a` to `b` and a functor holding an `a`, and return a functor holding a `b`."

Examples:
haskell
-- With Maybe
(+1) <$> Just 5   -- Result: Just 6
(+1) <$> Nothing  -- Result: Nothing

-- With List: applies to every element
(+1) <$> [1, 2, 3] -- Result: [2, 3, 4]

-- With IO: gets result from IO, applies function, puts it back in IO
-- (getLine returns IO String, so toUpper <$> getLine returns IO String)
toUpper <$> getLine -- If you type "hi", it becomes "HI" inside the IO action

2. <*> - The Applicative Operator

What it is: Applies a function inside a context to a value inside a context. This is the key differentiator from Functor. Pronounced "apply".

Type Signature:
haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

"Take a functor holding a function from `a` to `b` and a functor holding an `a`, and return a functor holding a `b`."

Examples:
haskell
-- With Maybe: apply only if both sides are "successful" (Just)
Just (+3) <*> Just 5  -- Result: Just 8
Just (+3) <*> Nothing -- Result: Nothing
Nothing  <*> Just 5   -- Result: Nothing

-- With List: apply every function to every value (Cartesian product)
[(+1), (*2)] <*> [3, 4] -- Result: [4, 5, 6, 8]  i.e., [3+1, 4+1, 3*2, 4*2]

-- Common pattern: use <$> to lift a function, then <*> to apply its arguments
-- This is like calling a multi-argument function in a context
(\x y -> x + y) <$> Just 1 <*> Just 2 -- Result: Just 3

3. >>= (bind) - The Monad Operator

What it is: The essence of Monad. It sequences computations. It takes a value in a context, extracts it, and feeds it to a function that returns a new context. This allows subsequent computations to depend on the results of previous ones.

Type Signature:
haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b

"Take a monadic value holding an `a` and a function from a plain `a` to a monadic `b`, and return a monadic `b`."

Examples:
haskell
-- With Maybe: chain computations that might fail
half :: Int -> Maybe Int
half x = if even x then Just (x `div` 2) else Nothing

Just 20 >>= half -- Result: Just 10
Just 10 >>= half -- Result: Just 5
Just 5 >>= half  -- Result: Nothing (5 is not even)

-- With IO: sequence actions where the next depends on the previous
-- getLine >>= (\input -> putStrLn ("You said: " ++ input))
main = getLine >>= putStrLn . ("You said: " ++)

4. pure / return

What they are: They both take a pure value and lift it into a minimal computational context. They are the same function (`return = pure`), with `pure` being the preferred modern term (from Applicative).

Type Signature:
haskell
pure :: Applicative f => a -> f a
return :: Monad m => a -> m a

"Take any value of type `a` and return it wrapped in the Applicative/Monad context."

Examples:
haskell
pure 5 :: Maybe Int    -- Result: Just 5
pure 5 :: [Int]        -- Result: [5]  (the simplest list)
return "hello" :: IO String -- Result: IO action that yields "hello"

5. >> (then) - The Monad Operator for Sequencing

What it is: Used to sequence two monadic actions when you don't care about the result of the first one. It simply runs the first action for its side effects and then runs the second.

Type Signature:
haskell
(>>) :: Monad m => m a -> m b -> m b

"Take one monadic action, then another, and return the result of the second."

Examples:
haskell
-- With IO: print something, then get input, ignoring the result of putStrLn
putStrLn "What is your name?" >> getLine

-- This is equivalent to the do-block:
do
  putStrLn "What is your name?"
  getLine

-- With Maybe: run first computation, and if it's Just, run the second.
-- The value of the first Just is thrown away.
Just 1 >> Just 2  -- Result: Just 2
Just 1 >> Nothing -- Result: Nothing
Nothing >> Just 2 -- Result: Nothing (fails on first step)

Summary Table

| Operator | Type Class | Purpose | Key Idea |
| :--- | :--- | :--- | :--- |
| **`<$>`** | `Functor` | Apply function to value in context. | `(a -> b) -> f a -> f b` |
| **`<*>`** | `Applicative`| Apply function in context to value in context. | `f (a -> b) -> f a -> f b` |
| **`>>=`** | `Monad` | Chain computations where next depends on previous result. | `m a -> (a -> m b) -> m b` |
| **`pure`/`return`**| `Applicative`/`Monad`| Lift a pure value into a context. | `a -> f a` |
| **`>>`** | `Monad` | Sequence actions, ignoring the result of the first. | `m a -> m b -> m b` |
