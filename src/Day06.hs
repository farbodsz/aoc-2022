--------------------------------------------------------------------------------

module Day06 (solveA, solveB) where

import Data.List (group, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Util

--------------------------------------------------------------------------------

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow _ [] = []
slidingWindow n xss@(_ : xs)
    | n <= length xss = take n xss : slidingWindow n xs
    | otherwise = slidingWindow n xs

findMarker :: Int -> [Char] -> Int
findMarker n =
    fst
        . head
        . dropWhile (not . snd)
        . zip [n ..]
        . map allDifferent
        . slidingWindow n
  where
    allDifferent = all ((== 1) . length) . group . sort

--------------------------------------------------------------------------------

solveA :: Text -> Text
solveA = tshow . findMarker 4 . T.unpack

solveB :: Text -> Text
solveB = tshow . findMarker 14 . T.unpack

--------------------------------------------------------------------------------
