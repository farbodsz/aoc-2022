--------------------------------------------------------------------------------

module Day09 (solveA, solveB) where

import Control.Monad (forM_, replicateM_, when)
import Control.Monad.State (State, execState, gets, modify)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector ((!), (//))
import Data.Vector qualified as V
import Util

--------------------------------------------------------------------------------
-- Direction

data Direction = U | R | D | L
    deriving (Read, Show)

data Move = Move {direction :: Direction, amount :: Int}
    deriving (Read, Show)

processInput :: Text -> [Move]
processInput = map processLine . T.lines
  where
    processLine t = case T.words t of
        [dir, amount] -> Move (tread dir) (tread amount)
        _ -> error "processInput: cannot read"

--------------------------------------------------------------------------------
-- Position

type Pos = (Int, Int)

posPlus :: Pos -> Pos -> Pos
posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | @getClosestPos src dest@ gives the closest position from @dest@ that is one
-- square from @src@.
getClosestPos :: Pos -> Pos -> Pos
getClosestPos srcPos destPos =
    snd $ minimum $ ((,) =<< (`distTo` destPos)) <$> surrounding srcPos
  where
    distTo p1 p2 = (pythagDistance p1 p2, p1)

surrounding :: Pos -> [Pos]
surrounding p = map (posPlus p) diffs
  where
    diffs = zip (concat $ replicate 3 ds) (concatMap (replicate 3) ds)
    ds = [-1, 0, 1]

pythagDistance :: Pos -> Pos -> Double
pythagDistance p1 p2 =
    let dx = fromIntegral $ abs $ fst p1 - fst p2
        dy = fromIntegral $ abs $ snd p1 - snd p2
     in sqrt (dx ** 2 + dy ** 2)

diagDistance :: Pos -> Pos -> Int
diagDistance (x1, y1) (x2, y2) = max (abs $ x1 - x2) (abs $ y1 - y2)

newPos :: Move -> Pos -> Pos
newPos (Move dir n) (x, y) = case dir of
    U -> (x, y + n)
    D -> (x, y - n)
    L -> (x - n, y)
    R -> (x + n, y)

--------------------------------------------------------------------------------
-- State

data St = St {knots :: V.Vector Pos, visited :: V.Vector (S.Set Pos)}
    deriving (Show)

initState :: Int -> St
initState num =
    St {knots = forEachKnot pos0, visited = forEachKnot (S.fromList [pos0])}
  where
    forEachKnot = V.replicate num
    pos0 = (0, 0)

-- | Moving a knot updates its position and saves it to the "visited" map.
modifyKnot :: Int -> Pos -> State St ()
modifyKnot idx pos = modify $ \state ->
    state
        { knots = state.knots // [(idx, pos)]
        , visited = state.visited // [(idx, S.insert pos (state.visited ! idx))]
        }

move :: Move -> State St ()
move m = replicateM_ m.amount $ moveOne m.direction

moveOne :: Direction -> State St ()
moveOne headDir = do
    -- Update head
    currHead <- gets $ (! 0) . (.knots)
    modifyKnot 0 $ newPos (Move headDir 1) currHead
    maxIdx <- gets $ subtract 1 . V.length . (.knots)

    -- Update subsequent knots in pairs of knots (x, y)
    forM_ (zip [0 .. maxIdx - 1] [1 .. maxIdx]) $ \(i, j) -> do
        currKnots <- gets (.knots)
        let posX = currKnots ! i
            posY = currKnots ! j
        when (diagDistance posX posY > 1) $ do
            let newPosY = getClosestPos posY posX
            modifyKnot j newPosY

--------------------------------------------------------------------------------
-- Solution

solveA :: Text -> Text
solveA = solveWith 2

solveB :: Text -> Text
solveB = solveWith 10

-- | Solves with number of knots
solveWith :: Int -> Text -> Text
solveWith n =
    tshow . S.size . (! tailKnot) . (.visited) . runProblem . processInput
  where
    runProblem ms = execState (mapM_ move ms) (initState n)
    tailKnot = n - 1

--------------------------------------------------------------------------------
