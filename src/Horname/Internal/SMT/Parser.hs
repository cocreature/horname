{-# LANGUAGE OverloadedStrings #-}
module Horname.Internal.SMT.Parser where

import           Data.Char
import           Data.Generics.Uniplate.Data
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import           Horname.Internal.SMT
import           Text.Megaparsec
import           Text.Megaparsec.Text

parseName :: Parser Text
parseName = Text.pack <$> some (noneOf [' ', '\n', ')'])

parseSort :: Parser Sort
parseSort = do
  c <- noneOf [' ', '\n', ')']
  case c of
    '(' -> do
      space
      args <- map (\(Sort t) -> t) <$> sepBy parseSort someSpace
      _ <- char ')'
      pure $ Sort ("(" <> Text.intercalate " " args <> ")")
    c' -> do
      rest <- many (noneOf [' ', '\n', ')'])
      pure (Sort (Text.pack (c' : rest)))

parseArg :: Parser Arg
parseArg = do
  _ <- char '('
  name <- parseName
  someSpace
  sort <- parseSort
  _ <- char ')'
  pure (Arg (VarName name) sort)

parseArgs :: Parser [Arg]
parseArgs = do
  _ <- char '('
  args <- sepBy parseArg someSpace
  _ <- char ')'
  pure args

someSpace :: Parser ()
someSpace = skipSome spaceChar

parseSExpr :: Parser SExpr
parseSExpr = do
  c <- char '(' <|> satisfy (not . isSpace)
  case c of
    '(' -> do
      space
      args <- sepBy parseSExpr someSpace
      space
      _ <- char ')'
      pure (List args)
    alphaNum
      | isDigit alphaNum -> do
        digits <- many digitChar
        pure (IntLit (read (alphaNum : digits)))
      | otherwise -> do
        rest <- many (noneOf [' ', ')'])
        pure . StringLit . Text.pack $ alphaNum : rest

parseDefineFun :: Parser DefineFun
parseDefineFun = do
  _ <- manyTill anyChar (string "(define-fun")
  someSpace
  name <- parseName
  someSpace
  args <- parseArgs
  someSpace
  retSort <- parseSort
  someSpace
  expr <- parseSExpr
  space
  _ <- char ')'
  pure
    (DefineFun
       name
       args
       retSort
       (transform simplify' . transform simplify $ inlineLets expr))

parseDefineFuns :: Parser [DefineFun]
parseDefineFuns = many (try parseDefineFun)

parseDeclareFun :: Parser (Text, [Text])
parseDeclareFun = do
  _ <- manyTill anyChar (string "; :annot (")
  name <- parseName
  someSpace
  args <-
    map (Text.replace "_0" "") <$>
    sepBy parseName someSpace
  space
  _ <- char ')'
  pure (name, args)

parseDeclareFuns :: Parser (Map Text [Text])
parseDeclareFuns = Map.fromList <$> many (try parseDeclareFun)
