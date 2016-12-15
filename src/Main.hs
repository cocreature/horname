{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as Text
import           Options
import           Options.Applicative hiding (Parser, runParser)
import           SMT
import           SMT.Parser
import           Text.Megaparsec
import           Text.Megaparsec.Text

main :: IO ()
main = do
  Opts outp inp <- execParser opts
  outContent <- Text.readFile outp
  let parseResult = runParser parseDefineFuns outp outContent
  case parseResult of
    Left err -> putStrLn (parseErrorPretty err)
    Right res -> do
      putStrLn "Successful parse\n"
      print res
  where
    opts = info (helper <*> optsParser) (header "horname")
