{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec as MP hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Byte.Lexer (symbol)
import Text.Parsec (anyChar)
import qualified Text.Parsec as Text.Megaparsec.Char
import Data.Char (isSpace)
import Data.Either (fromRight, rights)
import qualified Data.Set as S
import Data.Set (Set)

type Parser = Parsec Void String

sc = L.space space1 empty empty

lexeme = L.lexeme sc
integer = lexeme L.decimal
symb = L.symbol sc

type Name = String
type Size = Int
data Command = CommandChangeDir Name | CommandList deriving (Eq, Show)
data Content = Dir {name :: Name, contents :: Set Content} | File Name Size deriving (Eq, Ord, Show)
type Instruction = Either Command Content

data FSCrumb = FSCrumb Name (Set Content) (Set Content) deriving (Show)
type FSZipper = (Content, [FSCrumb])

cdTo :: Name -> FSZipper -> FSZipper
cdTo name (Dir folderName contents, bs) =
    let (ls, item:rs) = break (nameIs name) (S.toList contents)
    in  (item, FSCrumb folderName (S.fromList ls) (S.fromList rs):bs)

topMost :: FSZipper -> FSZipper
topMost (t, []) = (t,[])
topMost z = topMost (goUp z)

goUp :: FSZipper -> FSZipper
goUp (t, b:bs) = (Dir name (S.union (S.insert t c1) c2), bs) where
  FSCrumb name c1 c2 = b

nameIs :: Name -> Content -> Bool
nameIs name (Dir dirName _) = name == dirName
nameIs name (File fileName _) = name == fileName

fsNewFile :: Content -> FSZipper -> FSZipper
fsNewFile item (Dir folderName contents, bs) =
    (Dir folderName (S.insert item contents), bs)

rootDir = Dir "/" S.empty
rootZipper :: FSZipper
rootZipper = (rootDir, [])
child = Dir "child" (S.fromList [File "child dir file" 13])
next = fsNewFile child rootZipper

pCommand :: Parser Instruction
pCommand = choice
  [ command
  , dir
  , file
  ]
  where
    command = do
      symb "$"
      choice [changeDir, listDir]
    changeDir = do
      symb "cd"
      path <- symb ".." <|> symb "/" <|> content
      pure $ Left $ CommandChangeDir path
    listDir = Left CommandList <$ symb "ls"
    dir =  do
      symb "dir"
      name <- content
      pure $ Right $ Dir name S.empty
    file = do
      size <- integer
      name <- content
      pure $ Right $ File name size
    content = do
      r <- MP.many (satisfy (not . isSpace))
      void (char '\n') <|> eof
      return r

pInput :: Parser [Instruction]
pInput = MP.many pCommand

processCommand :: Command -> FSZipper -> FSZipper
processCommand c z = case c of
  CommandChangeDir "/" -> topMost z
  CommandChangeDir ".." -> goUp z
  CommandChangeDir name -> cdTo name z
  CommandList -> z

processContent = fsNewFile

process is = foldl (\z i -> case i of
 Left x -> processCommand x z
 Right x -> processContent x z
  ) rootZipper is

size (File _ size) = size
size (Dir _ cs) = foldr ((+) . size) 0 cs

isDir (Dir _ _) = True
isDir _ = False

getSubDirs dir = S.fromList $ concatMap (S.toList . getSubDirs) dirs ++ dirs where
  dirs = S.toList $ S.filter isDir (contents dir)
getSubDirs' dir isCandidate = S.filter isCandidate $ getSubDirs dir

main = do
    input <- readFile "input"
    case runParser (pInput <* eof) "" input of
      Right x -> do
        print $ findTotal $ getSubDirs' fs candidate1
        print $ findMin $ getSubDirs' fs candidate2
        where
          fs = fst . topMost $ process x
          usedSize = size fs
          candidate1 = (<= 100000) . size
          candidate2 c = size c >= 30000000 - (70000000 - usedSize)
          findTotal = foldr ((+) . size) 0
          findMin = foldr (min . size) maxBound
      _ -> print "parsing error"