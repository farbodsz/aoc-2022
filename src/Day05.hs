--------------------------------------------------------------------------------

module Day05 (solveA, solveB) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Util

--------------------------------------------------------------------------------

-- | Stack where the first list element = top of the stack.
newtype Stack a = Stack [a]
    deriving (Show)

peek :: Stack a -> a
peek (Stack []) = error "Empty list"
peek (Stack (x : _)) = x

pop :: Int -> Stack a -> (Stack a, [a])
pop n (Stack xs) = (Stack (drop n xs), reverse $ take n xs)

push :: Stack a -> [a] -> Stack a
push (Stack xs) ys = Stack (ys <> xs)

--------------------------------------------------------------------------------

data Move = Move {count :: Int, from :: Int, to :: Int}
    deriving (Show)

-- Always "move N from X to Y". Convert to be 0-indexed.
readMove :: Text -> Move
readMove t = case tread <$> T.words t of
    [_, n, _, x, _, y] -> Move n (x - 1) (y - 1)
    l -> error $ "Unexpected line " ++ show l

data Game a = Game {stacks :: [Stack a], opts :: GameOptions a}

newtype GameOptions a = GameOptions
    { popStrategy :: Int -> Stack a -> (Stack a, [a])
    }

-- 1. Lines have trailing spaces s.t. each line has same width, so easy to
--    determine number of columns/stacks.
-- 2. Read lines top to bottom, gradually building the list of stacks.
readGame :: GameOptions Char -> [Text] -> Game Char
readGame opts =
    flip Game opts . map (Stack . catMaybes) . transpose . map readLine . init
  where
    -- Character for each cell per column/stack if exists
    readLine :: Text -> [Maybe Char]
    readLine = map cellToChar . T.chunksOf 4

    cellToChar :: Text -> Maybe Char
    cellToChar cell = case T.head . T.drop 1 $ cell of
        ' ' -> Nothing
        ch -> Just ch

playGame :: Game a -> [Move] -> Game a
playGame = foldl nextState

nextState :: Game a -> Move -> Game a
nextState g m =
    let (st', xs) = g.opts.popStrategy m.count (g.stacks !! m.from)
        g' = g {stacks = applyTo m.from (const st') g.stacks}
        st'' = push (g'.stacks !! m.to) xs
     in g {stacks = applyTo m.to (const st'') g'.stacks}

-- | @applyTo n f xs@ applies @f@ to the @n@th element of @xs@
applyTo :: Int -> (a -> a) -> [a] -> [a]
applyTo 0 f (x : xs) = f x : xs
applyTo n f (x : xs) = x : applyTo (n - 1) f xs
applyTo _ _ [] = error "Cannot apply function to empty list"

--------------------------------------------------------------------------------

processInput :: GameOptions Char -> Text -> (Game Char, [Move])
processInput opts =
    bimap (readGame opts) (map readMove . tail) . break T.null . T.lines

-- | Combine top char of each stack
gameResult :: Game Char -> Text
gameResult g = T.pack $ peek <$> g.stacks

--------------------------------------------------------------------------------

solveA :: Text -> Text
solveA t =
    let (game, ms) = processInput optionsA t
     in gameResult $ playGame game ms

optionsA :: GameOptions Char
optionsA = GameOptions {popStrategy = pop}

--------------------------------------------------------------------------------

solveB :: Text -> Text
solveB t =
    let (game, ms) = processInput optionsB t
     in gameResult $ playGame game ms

optionsB :: GameOptions Char
optionsB = GameOptions {popStrategy = (fmap . fmap) reverse . pop}

--------------------------------------------------------------------------------
