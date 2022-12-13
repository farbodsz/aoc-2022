--------------------------------------------------------------------------------

module Day12 (solveA, solveB) where

import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Sequence (Seq, ViewL (..), (><))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Util

--------------------------------------------------------------------------------

type Grid a = Vector (Vector a)
type Pos = (Int, Int)
type Bounds = (Int, Int)
type Distance = Int

processInput :: Text -> Grid Char
processInput =
    V.fromList . map (V.fromList . map (head . T.unpack) . T.chunksOf 1) . T.lines

gridFind :: Eq a => Grid a -> a -> Maybe Pos
gridFind grid n = do
    y <- V.findIndex (\row -> n `V.elem` row) grid
    x <- V.findIndex (== n) (grid ! y)
    pure (x, y)

gridFindMany :: Eq a => Grid a -> a -> Vector Pos
gridFindMany grid n =
    let ys = V.findIndices (\row -> n `V.elem` row) grid
     in V.concatMap (\y -> (,y) <$> V.findIndices (== n) (grid ! y)) ys

gridBounds :: Grid a -> Bounds
gridBounds g = (V.length (V.head g) - 1, V.length g - 1)

around :: Bounds -> Pos -> [Pos]
around (maxX, maxY) (x, y) =
    filter
        (\(p, q) -> 0 <= p && p <= maxX && 0 <= q && q <= maxY)
        [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

canClimb :: Grid Char -> Pos -> Pos -> Bool
canClimb grid p q = ord from + 1 >= ord to
  where
    from =
        let from' = grid ! snd p ! fst p
         in if from' == 'S' then 'a' else from'
    to =
        let to' = grid ! snd q ! fst q
         in if to' == 'E' then 'z' else to'

neighbors :: Grid Char -> Set Pos -> Pos -> [Pos]
neighbors grid visited u =
    filter (canClimb grid u) $
        filter (`S.notMember` visited) $
            around (gridBounds grid) u

dijkstra :: Grid Char -> Pos -> Pos -> Maybe Distance
dijkstra grid src = go (S.singleton src) (Seq.singleton (src, 0))
  where
    go :: Set Pos -> Seq (Pos, Distance) -> Pos -> Maybe Distance
    go visited queue dest = case Seq.viewl queue of
        EmptyL -> Nothing
        (u, d) :< us ->
            if u == dest
                then Just d
                else
                    let vs = neighbors grid visited u
                        visited' = visited `S.union` S.fromList vs
                        toQueue = (,d + 1) <$> Seq.fromList vs
                     in go visited' (us >< toQueue) dest

--------------------------------------------------------------------------------
-- Solution

solveA :: Text -> Text
solveA = tshow . fromJust . go . processInput
  where
    go grid = do
        src <- gridFind grid 'S'
        dest <- gridFind grid 'E'
        dijkstra grid src dest

solveB :: Text -> Text
solveB = tshow . go . processInput
  where
    go grid =
        let src = fromJust $ gridFind grid 'S'
            dest = fromJust $ gridFind grid 'E'
            as = gridFindMany grid 'a'
            answers = V.map (\a -> dijkstra grid a dest) (V.cons src as)
         in minimum $ V.catMaybes answers

--------------------------------------------------------------------------------
