{-# LANGUAGE OverloadedStrings #-}
module SMT.Parser where

import           Data.Char
import           Data.Generics.Uniplate.Data
import           Data.Text (Text)
import qualified Data.Text as Text
import           SMT
import           Text.Megaparsec
import           Text.Megaparsec.Text

parseName :: Parser Text
parseName = Text.pack <$> some (noneOf [' ', '\n', ')'])

parseSort :: Parser Sort
parseSort = Sort . Text.pack <$> some (noneOf [' ', '\n', ')'])

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
  pure (DefineFun name args retSort (transform simplify $ inlineLets expr))

parseDefineFuns :: Parser [DefineFun]
parseDefineFuns = many (try parseDefineFun)
