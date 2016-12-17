{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as Text
import           Horname
import           Options
import           Options.Applicative hiding (Parser, runParser)

main :: IO ()
main = do
  Opts outp inp <- execParser opts
  outContent <- Text.readFile outp
  inContent <- Text.readFile inp
  case extractRenamedInvariants inp inContent outp outContent of
    Left errs -> print errs
    Right invariants -> mapM_ (Text.putStrLn . ppDefineFun) invariants
  where
    opts = info (helper <*> optsParser) (header "horname")
