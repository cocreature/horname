module Horname
  ( extractRenamedInvariants
  , DefineFun(..)
  , ppDefineFun
  , SExpr(..)
  , VarName(..)
  , Sort(..)
  , Arg(..)
  ) where

import           Data.Text (Text)
import qualified Data.Text.Lazy as Text
import           Data.These
import           Horname.Internal.SMT
import           Horname.Internal.SMT.Parser
import qualified Horname.Internal.SMT.Pretty as Pretty
import           Text.Megaparsec
import           Text.PrettyPrint.Leijen.Text

extractRenamedInvariants
  :: FilePath {- ^ input filename -}
  -> Text {- ^ solver input -}
  -> FilePath {- ^ output filename -}
  -> Text {- ^ solver output -}
  -> Either (These String String) [DefineFun]
extractRenamedInvariants inpFile inp outpFile outp =
  let outpResult = runParser parseDefineFuns outpFile outp
      inpResult = runParser parseDeclareFuns inpFile inp
  in case (outpResult, inpResult) of
       (Left err, Left err') ->
         Left (These (parseErrorPretty err) (parseErrorPretty err'))
       (Left err, _) -> Left (This (parseErrorPretty err))
       (_, Left err) -> Left (That (parseErrorPretty err))
       (Right defineFuns, Right declareFuns) ->
         Right (extractDefinitions declareFuns defineFuns)

-- The magi numbers here are taken from hPutDoc
ppDefineFun :: DefineFun -> Text
ppDefineFun = Text.toStrict . displayT . renderPretty 0.4 80 . Pretty.ppDefineFun
