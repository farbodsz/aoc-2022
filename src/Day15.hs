--------------------------------------------------------------------------------

module Day15 (solveA, solveB) where

import Data.List (sort)
import Data.Maybe (fromJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Util

--------------------------------------------------------------------------------
-- Types

type Pos = (Int, Int)

data Range = Range {start :: Int, end :: Int}
    deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Parsing

-- | @processLine line@ parses @line@ to get @(sensorPos, closestBeaconPos)@
processLine :: Text -> (Pos, Pos)
processLine t = fromJust . flip (parseMaybe @(Pos, Pos)) t $ do
    sensorX <- string "Sensor at x=" *> signed space decimal <* string ", "
    sensorY <- string "y=" *> signed space decimal <* string ": "
    beaconX <-
        string "closest beacon is at x=" *> signed space decimal <* string ", "
    beaconY <- string "y=" *> signed space decimal
    pure ((sensorX, sensorY), (beaconX, beaconY))

-- | @processInput input@ returns list of @(sensorPos, manhattanDistance)@ pairs
processInput :: Text -> [(Pos, Int)]
processInput = map ((\(s, b) -> (s, manhattan s b)) . processLine) . T.lines

--------------------------------------------------------------------------------
-- Main logic

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

coverageAt :: Int -> (Pos, Int) -> Maybe Range
coverageAt targetY ((srcX, srcY), dist)
    | k < 0 = Nothing
    | otherwise = Just $ Range (srcX - k) (srcX + k)
  where
    k = dist - abs (targetY - srcY)

groupRanges :: [Range] -> [Range]
groupRanges = foldl mergeRange [] . sort
  where
    mergeRange [] r = [r]
    mergeRange (r : rs) s
        | r.end >= s.start = Range r.start (max r.end s.end) : rs
        | otherwise = r : mergeRange rs s

countInRanges :: [Range] -> Int
countInRanges = foldr (\r acc -> (r.end - r.start) + acc) 0

--------------------------------------------------------------------------------
-- Solution

solveA :: Text -> Text
solveA =
    tshow
        . countInRanges
        . groupRanges
        . mapMaybe (coverageAt targetY)
        . processInput
  where
    targetY = 2000000

solveB :: Text -> Text
solveB = undefined

--------------------------------------------------------------------------------
