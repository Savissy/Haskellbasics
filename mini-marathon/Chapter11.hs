--Write a program that asks the user for their name and prints a greeting.

main :: IO ()
main = do
 print "Whats your name"
 print "Nice having you with us"

--Write a program that asks the user for a number, reads it, and prints that number multiplied by 2

multi :: Int -> Int
multi x = x * 2

main :: IO ()
main = do
 putStrLn "Whats your number?"
 x <- getLine
 let y = read x :: Int
 print $ multi y

--Write a program that asks the user for a number, reads it, and prints that number multiplied by 2.
main :: IO ()
main = do
    putStrLn "Send a number and ill multiply it by 2"
    name <- getLine
    let x = read name :: Int
    print $ (x * 2)
