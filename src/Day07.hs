--------------------------------------------------------------------------------

module Day07 (solveA, solveB) where

import Control.Monad (liftM2)
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, many, parseMaybe, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, eol, space1, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Util

--------------------------------------------------------------------------------

type Name = Text
type Size = Int

data LsFile = LsDir Name | LsFile Size Name
data Command = CdRoot | CdUp | CdInto Name | Ls [LsFile]

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

processInput :: Text -> [Command]
processInput t = fromJust $ parseMaybe parser t

parser :: Parser [Command]
parser = some $ choice [cdRootP, cdUpP, cdIntoP, lsP]
  where
    cdIntoP = fmap CdInto (string "$ cd " *> nameP <* eol)
    cdRootP = string "$ cd /" *> eol >> pure CdRoot
    cdUpP = string "$ cd .." >> eol >> pure CdUp
    lsP = fmap Ls ((string "$ ls" >> eol) *> many (fileP <* eol))
    fileP =
        fmap LsDir (string "dir " *> nameP)
            <|> liftM2 LsFile decimal (space1 *> nameP)
    nameP = T.pack <$> some (alphaNumChar <|> char '.')

--------------------------------------------------------------------------------

type Path = [Name]
data File = Dir Name | File Size Name
type FileSystem = M.Map Path [File]

mkFileSystem :: [Command] -> FileSystem
mkFileSystem cmds = go cmds ["/"] M.empty
  where
    go [] _ fs = fs
    go (CdRoot : cs) _ fs = go cs ["/"] fs
    go (CdUp : cs) cwd fs = go cs (init cwd) fs
    go ((CdInto dir) : cs) cwd fs = go cs (cwd ++ [dir]) fs
    go ((Ls xs) : cs) cwd fs = go cs cwd (M.insert cwd files fs)
      where
        files = map asFile xs
        asFile (LsDir name) = Dir name
        asFile (LsFile size name) = File size name

computeSizes :: FileSystem -> [(Path, Size)]
computeSizes fs = map (\d -> (d, fileSize fs d)) $ M.keys fs

fileSize :: FileSystem -> Path -> Size
fileSize fs path = sum $ map go (fs ! path)
  where
    go (Dir dir) = fileSize fs (path ++ [dir])
    go (File sz _) = sz

--------------------------------------------------------------------------------
-- Solutions

solveA :: Text -> Text
solveA =
    tshow
        . sum
        . filter (<= 100000)
        . map snd
        . computeSizes
        . mkFileSystem
        . processInput

solveB :: Text -> Text
solveB = undefined

--------------------------------------------------------------------------------
