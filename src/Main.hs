{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid
import qualified Data.Text.IO as Text
import           Options
import           Options.Applicative hiding (Parser, runParser)
import           SMT
import           SMT.Parser
import           SMT.Pretty
import           Text.Megaparsec
import           Text.PrettyPrint.Leijen.Text (Doc, putDoc, line)

putDocLn :: Doc -> IO ()
putDocLn d = putDoc (d <> line)

main :: IO ()
main = do
  Opts outp inp <- execParser opts
  outContent <- Text.readFile outp
  let parseResult = runParser parseDefineFuns outp outContent
  case parseResult of
    Left err -> putStrLn (parseErrorPretty err)
    Right defineFuns -> do
      putStrLn "Successful parse\n"
      mapM_ (putDocLn . ppDefineFun) defineFuns
  where
    opts = info (helper <*> optsParser) (header "horname")
