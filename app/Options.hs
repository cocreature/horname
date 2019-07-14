module Options
  ( Opts(..)
  , optsParser
  ) where

import Options.Applicative

data Opts = Opts
  { solverOuptutFile :: !FilePath
  , solverInputFile :: !FilePath
  } deriving (Show, Eq, Ord)

optsParser :: Parser Opts
optsParser =
  Opts <$>
  strOption (long "smt-output" <> help "Path to the solver output") <*>
  strOption (long "smt-input" <> help "Path to the input smt file")
