{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Options.Applicative hiding (Parser)
import qualified Options.Applicative as OptParse
import           Text.Megaparsec
import           Text.Megaparsec.Text

data Opts = Opts
  { solverOuptutFile :: !FilePath
  , solverInputFile :: !FilePath
  } deriving (Show, Eq, Ord)

optParser :: OptParse.Parser Opts
optParser =
  Opts <$> strOption (long "smt-output" <> help "Path to the solver output") <*>
  strOption (long "smt-input" <> help "Path to the input smt file")

newtype Sort =
  Sort Text
  deriving (Show, Eq, Ord)

data Arg = Arg
  { argName :: !Text
  , argSort :: !Sort
  } deriving (Show, Eq, Ord)

data DefineFun = DefineFun
  { arguments :: ![Arg]
  , returnSort :: !Sort
  , body :: !Text
  } deriving (Show, Eq, Ord)

parseName :: Parser Text
parseName = T.pack <$> some (noneOf [' ', ')'])

parseSort :: Parser Sort
parseSort = Sort . T.pack <$> some (noneOf [' ',')'])

parseArg :: Parser Arg
parseArg = do
  _ <- char '('
  name <- parseName
  sort <- parseSort
  _ <- char ')'
  pure (Arg name sort)

parseArgs :: Parser [Arg]
parseArgs = do
  _ <- char '('
  args <- sepBy parseArg space
  _ <- char ')'
  pure args

parseDefineFun :: Parser DefineFun
parseDefineFun = do
  _ <- manyTill anyChar (string "(define-fun")
  _ <- string "(define-fun"
  space
  args <- parseArgs
  space
  retSort <- parseSort
  pure (DefineFun args retSort "")

main :: IO ()
main = do
  Opts outp inp <- execParser opts
  putStrLn inp
  putStrLn outp
  where
    opts = info (helper <*> optParser) (header "horname")
