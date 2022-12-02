--------------------------------------------------------------------------------

module Day01 (solveA, solveB) where

import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as T
import Util

--------------------------------------------------------------------------------

solveA :: Text -> Text
solveA = tshow . maximum . groupNums

groupNums :: Text -> [Int]
groupNums = map (sum . map tread) . splitLines . T.lines

splitLines :: [Text] -> [[Text]]
splitLines ls =
    let (xs, ys) = span (/= "") ls
     in case ys of
            [] -> [xs]
            _ : ys' -> xs : splitLines ys'

--------------------------------------------------------------------------------

solveB :: Text -> Text
solveB = tshow . sum . take 3 . sortOn Down . groupNums

--------------------------------------------------------------------------------
