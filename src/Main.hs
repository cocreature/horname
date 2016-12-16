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
  inContent <- Text.readFile inp
  let outpResult = runParser parseDefineFuns outp outContent
      inpResult = runParser parseDeclareFuns inp inContent
  case (outpResult, inpResult) of
    (Left err, _) -> putStrLn (parseErrorPretty err)
    (_, Left err) -> putStrLn (parseErrorPretty err)
    (Right defineFuns, Right declareFuns) -> do
      mapM_ (putDocLn . ppDefineFun) (extractDefinitions declareFuns defineFuns)
  where
    opts = info (helper <*> optsParser) (header "horname")
