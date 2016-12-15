module SMT where

import Data.Text (Text)

newtype VarName =
  VarName Text
  deriving (Show, Eq, Ord)

newtype Sort =
  Sort Text
  deriving (Show, Eq, Ord)

data Arg = Arg
  { argName :: !VarName
  , argSort :: !Sort
  } deriving (Show, Eq, Ord)

data DefineFun = DefineFun
  { funName :: !Text
  , arguments :: ![Arg]
  , returnSort :: !Sort
  , body :: !SExpr
  } deriving (Show, Eq, Ord)

data SExpr
  = IntLit !Integer
  | Var !VarName
  | List ![SExpr]
  deriving (Show, Eq, Ord)
