import System.Directory (listDirectory)
import System.IO

main :: IO ()
main = do
  putStrLn "Listing all files and directories in the current directory:"
  files <- listDirectory "."
  mapM_ putStrLn files

import System.Directory (listDirectory)
import Data.List (isInfixOf)
import System.IO

-- Filter filenames containing a given substring
filterFilesBySubstring :: String -> [FilePath] -> [FilePath]
filterFilesBySubstring substring = filter (isInfixOf substring)

main :: IO ()
main = do
  putStrLn "Enter a substring to filter files:"
  substring <- getLine
  allFiles <- listDirectory "."
  let matchedFiles = filterFilesBySubstring substring allFiles
  putStrLn "\nFiltered files:"
  mapM_ putStrLn matchedFiles

import System.Directory (listDirectory)
import Data.List (isInfixOf, sort)

-- Filter and sort files based on a substring
filterAndSortFiles :: String -> [FilePath] -> [FilePath]
filterAndSortFiles substring = sort . filter (isInfixOf substring)

main :: IO ()
main = do
  putStrLn "Enter a substring to filter files:"
  substring <- getLine
  allFiles <- listDirectory "."
  let matchedFiles = filterAndSortFiles substring allFiles
  putStrLn "\nFiltered and sorted files:"
  mapM_ putStrLn matchedFiles

module SumNonEmpty (sumNonEmpty) where

-- | Sums a non-empty list.
-- Throws an error if the list is empty.
sumNonEmpty :: Num a => [a] -> a
sumNonEmpty [] = error "sumNonEmpty: Cannot sum an empty list"
sumNonEmpty xs = sum xs

main :: IO ()
main = do
    print $ sumNonEmpty [1,2,3,4,5]
    print $ sumNonEmpty []

module SumNonEmpty (sumNonEmpty) where

-- | Sums a non-empty list.
-- Throws an error if the list is empty.
sumNonEmpty :: Num a => [a] -> a
sumNonEmpty [] = error "sumNonEmpty: Cannot sum an empty list"
sumNonEmpty xs = sum xs

-- Private helper, not exported
emptyListMsg :: String
emptyListMsg = "Cannot sum an empty list."

main :: IO ()
main = do
  putStrLn emptyListMsg

import System.Directory (listDirectory)
import Data.List (isInfixOf)
import qualified Data.Map as Map

-- Filters files by a substring
filterFiles :: String -> [FilePath] -> [FilePath]
filterFiles keyword = filter (isInfixOf keyword)

-- Converts a list of file names to a map (fileName -> index)
fileMap :: [FilePath] -> Map.Map FilePath Int
fileMap files = Map.fromList (zip files [0..])

-- Main function to demonstrate
main :: IO ()
main = do
    putStrLn "Enter a keyword to filter files:"
    keyword <- getLine
    allFiles <- listDirectory "."
    let filtered = filterFiles keyword allFiles
        resultMap = fileMap filtered
    putStrLn "Filtered file map:"
    print resultMap
