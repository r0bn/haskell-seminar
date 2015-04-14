module Parser
( getCols
, getRows
, getMatrix
, getVector
, loadMatrix
) where

import Control.Monad
import Control.Exception
import Data.Char (isSpace)


main = do
    x <- loadMatrix "test_matrix_3x3.csv"
    let cols = getCols x
    let rows = getRows x
    print cols
    print rows
    print $ getMatrix x cols rows
    print $ getVector x cols


getVector :: [[String]] -> Int -> [Float]
getVector d size = assert (length d==2+2*size) [checkAndConvertVectorElement v | v <- slice d (2+size) (2+2*size-1)]

checkAndConvertVectorElement :: [String] -> Float
checkAndConvertVectorElement v = assert(length v==1) (read (v!!0) :: Float)

getMatrix :: [[String]] -> Int -> Int -> [[Float]]
getMatrix d rows cols = assert (rows==cols) checkAndConvertMatrix (slice d 2 (2+rows-1)) rows

checkAndConvertMatrix :: [[String]] -> Int -> [[Float]]
checkAndConvertMatrix d size = assert (length d==size) [checkAndConvertRow r size| r <- d]

checkAndConvertRow :: [String] -> Int -> [Float]
checkAndConvertRow r size = assert (length r==size) [read v::Float | v <- r]

getCols :: [[String]] -> Int
getCols d = assert (length(d!!0)==1) (read ((d!!0)!!0)::Int)

getRows :: [[String]] -> Int
getRows d = assert (length(d!!1)==1) (read ((d!!1)!!0)::Int)

loadMatrix :: String -> IO [[String]]
loadMatrix fn = do
    text <- readFile fn 
    return $ toCells $ removeDeadLines $ lines text

removeDeadLines :: [String] -> [String]
removeDeadLines text = [l | l <- text, (trim l) /= "", head l /= '#']

toCells :: [String] -> [[String]]
toCells lins = [[w | w <- words $ sepToSpace l] | l <- lins]

sepToSpace :: String -> String
sepToSpace s = [if c==',' then ' ' else c | c <-s]

trim :: String -> String
trim = f . f 
    where f =reverse . dropWhile isSpace

slice dat from to = take (to - from + 1) (drop from dat)
