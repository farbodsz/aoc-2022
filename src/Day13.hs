--------------------------------------------------------------------------------

module Day13 (solveA, solveB) where

import Data.List (sort)
import Data.List.Extra (chunksOf)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, parseMaybe, sepBy1, (<|>))
import Text.Megaparsec.Char (char, digitChar, string)
import Util

--------------------------------------------------------------------------------
-- Packet

data Packet = PItem Int | PList [Packet]
    deriving (Eq, Show)

instance Ord Packet where
    compare (PItem x) (PItem y) = compare x y
    compare (PList xs) (PList ys) = compare xs ys
    compare p@(PItem _) q@(PList _) = compare (PList [p]) q
    compare p@(PList _) q@(PItem _) = compare p (PList [q])

--------------------------------------------------------------------------------
-- Parsing

type Parser = Parsec Void Text

packetGroups :: Int -> Text -> [[Packet]]
packetGroups n = map (map parse) . chunksOf n . filter (not . T.null) . T.lines

parse :: Text -> Packet
parse = fromJust . parseMaybe packetP
  where
    packetP :: Parser Packet
    packetP = listP <|> itemP
      where
        itemP = PItem . read <$> many digitChar
        listP =
            PList
                <$> ( (string "[]" >> pure [])
                        <|> (char '[' *> packetP `sepBy1` char ',' <* char ']')
                    )

--------------------------------------------------------------------------------

solveA :: Text -> Text
solveA =
    tshow
        . sum
        . map fst
        . filter (\(_, ps) -> sort ps == ps)
        . zip @Int [1 ..]
        . packetGroups 2

solveB :: Text -> Text
solveB = undefined

--------------------------------------------------------------------------------
