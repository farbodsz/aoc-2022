--------------------------------------------------------------------------------

module Day04 (solveA, solveB) where

import Data.List (intersect)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple.Extra (both)
import Util

--------------------------------------------------------------------------------

data Range a = Range a a

rin :: Ord a => Range a -> Range a -> Bool
rin (Range x1 x2) (Range y1 y2) = x1 <= y1 && y2 <= x2

roverlap :: (Enum a, Eq a) => Range a -> Range a -> Bool
roverlap (Range x1 x2) (Range y1 y2) =
    not $ null $ intersect [x1 .. x2] [y1 .. y2]

--------------------------------------------------------------------------------

processInput :: Text -> [(Range Int, Range Int)]
processInput = map processLine . T.lines

processLine :: Text -> (Range Int, Range Int)
processLine = both readRange . toTuple . T.splitOn ","
  where
    readRange :: Text -> Range Int
    readRange = uncurry Range . toTuple . map tread . T.splitOn "-"

    toTuple (x : y : _) = (x, y)
    toTuple _ = error "Bad list"

--------------------------------------------------------------------------------

solveA :: Text -> Text
solveA =
    tshow
        . length
        . filter (== True)
        . map (\(r1, r2) -> r1 `rin` r2 || r2 `rin` r1)
        . processInput

--------------------------------------------------------------------------------

solveB :: Text -> Text
solveB =
    tshow
        . length
        . filter (== True)
        . map (uncurry roverlap)
        . processInput

--------------------------------------------------------------------------------
