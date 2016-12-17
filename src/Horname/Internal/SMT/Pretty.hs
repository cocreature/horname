{-# LANGUAGE OverloadedStrings #-}
module Horname.Internal.SMT.Pretty where

import           Data.Text (Text)
import qualified Data.Text.Lazy as Text
import           Horname.Internal.SMT
import           Text.PrettyPrint.Leijen.Text

dontIndent :: [Text]
dontIndent = ["+", "-", "*", "=", "<=", "<", ">=", ">"]

ppSExpr :: SExpr -> Doc
ppSExpr (IntLit i) = text . Text.pack $ show i
ppSExpr (StringLit s) = text . Text.fromStrict $ s
ppSExpr (List args'@(StringLit name:args))
  | name `elem` dontIndent = parens $ hsep (map ppSExpr args')
  | otherwise =
    parens $ text (Text.fromStrict name) <+> align (vsep (map ppSExpr args))
ppSExpr (List args) =
  parens $ align (vsep (map ppSExpr args))

ppSort :: Sort -> Doc
ppSort (Sort t) = text (Text.fromStrict t)

ppVarName :: VarName -> Doc
ppVarName (VarName t) = text (Text.fromStrict t)

ppArg :: Arg -> Doc
ppArg (Arg name sort) = parens (ppVarName name <+> ppSort sort)

ppDefineFun :: DefineFun -> Doc
ppDefineFun (DefineFun name args retType expr) =
  parens $
  text "define-fun" <+>
  align
    (vsep
       [ text (Text.fromStrict name)
       , parens (hsep (map ppArg args)) <+> ppSort retType
       , ppSExpr expr
       ])
