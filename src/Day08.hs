--------------------------------------------------------------------------------

module Day08 (solveA, solveB) where

import Data.List (transpose)
import Data.Text (Text)
import Data.Text qualified as T
import Util

--------------------------------------------------------------------------------

type Grid a = [[a]]

processInput :: Text -> Grid Int
processInput = map (map tread . T.chunksOf 1) . T.lines

visibility :: a -> ((Int, a) -> Int -> (Int, a)) -> Grid Int -> Grid a
visibility z scanner = map (map snd . go)
  where
    go = tail . scanl scanner (-1, z)

visibilities ::
    a ->
    ((Int, a) -> Int -> (Int, a)) ->
    (a -> a -> a) ->
    Grid Int ->
    Grid a
visibilities z scanner combiner g =
    foldr1 gridCombine $
        zipWith (\f f' -> f' (visibility z scanner (f g))) views views'
  where
    -- LTR, RTL, TTB, BTT
    views = [id, map reverse, transpose, map reverse . transpose]
    views' = [id, map reverse, transpose, transpose . map reverse]
    gridCombine = zipWith (zipWith combiner)

count :: (a -> Bool) -> [a] -> Int
count p xs = length $ filter p xs

--------------------------------------------------------------------------------

solveA :: Text -> Text
solveA =
    tshow
        . count (== True)
        . concat
        . visibilities True scanner combiner
        . processInput
  where
    scanner (peak, _) curr = (max peak curr, peak < curr)
    combiner = (||)

solveB :: Text -> Text
solveB = undefined

-- solveB = visibilities 0  . processInput
--     where
--         scanner (peak, _) curr = (max peak curr, peak < curr)

--------------------------------------------------------------------------------
