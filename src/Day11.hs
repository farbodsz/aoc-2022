--------------------------------------------------------------------------------

module Day11 (solveA, solveB) where

import Control.Monad.Cont
import Control.Monad.State (State, execState, gets, modify)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Ord (Down (Down))
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepBy, some, (<|>))
import Text.Megaparsec.Char (char, eol, numberChar, space1, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Util

--------------------------------------------------------------------------------

type Monkey = Int
type Item = Int

data Test = Test
    { divisibleBy :: Int
    , ifTrue :: Monkey
    , ifFalse :: Monkey
    }

data MonkeyInfo = MonkeyInfo
    { monkey :: Monkey
    , startingItems :: [Item]
    , operation :: Item -> Item
    , test :: Test
    }

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

processInput :: Text -> [MonkeyInfo]
processInput = fromJust . parseMaybe (parseMonkey `sepBy` eol)

parseMonkey :: Parser MonkeyInfo
parseMonkey = do
    monkeyNum <- string "Monkey " *> decimal <* char ':' <* eol
    items <- space1 *> "Starting items: " *> decimal `sepBy` string ", " <* eol
    op <- space1 *> "Operation: new = old " *> parseOperation <* eol
    testDiv <- space1 *> string "Test: divisible by " *> decimal <* eol
    testT <- space1 *> string "If true: throw to monkey " *> decimal <* eol
    testF <- space1 *> string "If false: throw to monkey " *> decimal <* eol
    pure $ MonkeyInfo monkeyNum items op (Test testDiv testT testF)

parseOperation :: Parser (Item -> Item)
parseOperation = do
    lhs <- (char '*' <|> char '+') <* space1
    rhs <- string "old" <|> T.pack <$> some numberChar
    let f = case lhs of
            '*' -> (*)
            '+' -> (+)
            _ -> error "parseOperation: cannot parse `f`"
    pure $ case rhs of
        "old" -> \old -> old `f` old
        x -> \old -> old `f` tread x

--------------------------------------------------------------------------------

type ListMap k v = M.Map k (Seq.Seq v)

lmAdd :: Ord k => k -> v -> ListMap k v -> ListMap k v
lmAdd k v mp = M.insert k ((mp ! k) |> v) mp

lmRemove :: (Ord k, Ord v) => k -> v -> ListMap k v -> ListMap k v
lmRemove k v mp = M.insert k (Seq.filter (/= v) (mp ! k)) mp

--------------------------------------------------------------------------------

data Game = Game
    { details :: M.Map Monkey MonkeyInfo
    , holdings :: ListMap Monkey Item
    , inspections :: M.Map Monkey Int
    }

type GameM = State Game

initGame :: [MonkeyInfo] -> Game
initGame mis =
    Game
        { details = M.fromList $ map (\mi -> (mi.monkey, mi)) mis
        , holdings =
            M.fromList $
                map (\mi -> (mi.monkey, Seq.fromList mi.startingItems)) mis
        , inspections = M.fromList $ map (\mi -> (mi.monkey, 0)) mis
        }

inspectItem :: Monkey -> GameM ()
inspectItem monkey = modify $ \game ->
    game {inspections = M.insertWith (+) monkey 1 game.inspections}

takeItem :: Monkey -> Item -> GameM ()
takeItem from item = modify $ \game ->
    game {holdings = lmRemove from item game.holdings}

passItem :: Monkey -> Item -> GameM ()
passItem to item = modify $ \game ->
    game {holdings = lmAdd to item game.holdings}

getMonkeyInfo :: Monkey -> GameM MonkeyInfo
getMonkeyInfo monkey = gets $ (! monkey) . (.details)

getMonkeyItems :: Monkey -> GameM [Item]
getMonkeyItems monkey = gets $ toList . (! monkey) . (.holdings)

playItem :: Monkey -> Item -> GameM ()
playItem monkey item = do
    -- 1. Inspect item
    inspectItem monkey
    -- 2. Worry level operation, then divide by 3
    info <- getMonkeyInfo monkey
    let newItem = info.operation item `div` 3
    -- 3. Apply test and pass item to next monkey
    let nextMonkey =
            if newItem `mod` info.test.divisibleBy == 0
                then info.test.ifTrue
                else info.test.ifFalse
    takeItem monkey item
    passItem nextMonkey newItem

playTurn :: Monkey -> GameM ()
playTurn monkey = do
    items <- getMonkeyItems monkey
    forM_ items $ playItem monkey

playRound :: GameM ()
playRound = do
    monkeys <- gets $ M.keys . (.details)
    forM_ monkeys playTurn

playGame :: Int -> Game -> Game
playGame rounds = execState (forM_ [1 .. rounds] (const playRound))

--------------------------------------------------------------------------------

solveA :: Text -> Text
solveA t = tshow $ product $ highestN 2 $ map snd $ M.toList finalInspections
  where
    highestN n = take n . sortOn Down
    finalInspections = (.inspections) $ playGame rs $ initGame $ processInput t
    rs = 20

--------------------------------------------------------------------------------

solveB :: Text -> Text
solveB = undefined

--------------------------------------------------------------------------------
