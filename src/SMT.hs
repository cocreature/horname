{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module SMT where

import           Data.Data
import           Data.List (foldl', partition)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)

newtype VarName =
  VarName Text
  deriving (Show, Eq, Ord, Data)

newtype Sort =
  Sort Text
  deriving (Show, Eq, Ord, Data)

data Arg = Arg
  { argName :: !VarName
  , argSort :: !Sort
  } deriving (Show, Eq, Ord, Data)

data DefineFun = DefineFun
  { funName :: !Text
  , arguments :: ![Arg]
  , returnSort :: !Sort
  , body :: !SExpr
  } deriving (Show, Eq, Ord, Data)

data SExpr
  = IntLit !Integer
  | StringLit !Text
  | List ![SExpr]
  deriving (Show, Eq, Ord, Data)

insertBindings :: Map Text SExpr -> [SExpr] -> Map Text SExpr
insertBindings m bindings = foldl' insertBinding m bindings

insertBinding :: Map Text SExpr -> SExpr -> Map Text SExpr
insertBinding m (List [StringLit key, val]) = Map.insert key val m
insertBinding _ _ = error "Syntax error in let bindings"

-- | This is not correct in the presence of nested lets but these
-- should never occur in the Z3 output and Eldarica doesn’t include
-- any lets.
inlineLets :: SExpr -> SExpr
inlineLets = inlineLets' Map.empty

inlineLets' :: Map Text SExpr -> SExpr -> SExpr
inlineLets' _ (IntLit i) = IntLit i
inlineLets' m (StringLit t) =
  case Map.lookup t m of
    Just val -> val
    Nothing -> StringLit t
inlineLets' m (List [StringLit "let", List bindings, expr]) =
  inlineLets' (insertBindings m bindings) expr
inlineLets' m (List args) = List (map (inlineLets' m) args)

simplify :: SExpr -> SExpr
-- (* (- 1) x) → x
simplify (List [StringLit "*", List [StringLit "-", IntLit 1], expr]) =
  List [StringLit "-", expr]
-- merge nested ands
simplify (List (StringLit "and":args)) =
  List (StringLit "and" : (andArgs ++ others))
  where
    (ands, others) =
      partition
        (\case
           List (StringLit "and":_) -> True
           _ -> False)
        args
    andArgs = extractAndArgs =<< ands
    extractAndArgs :: SExpr -> [SExpr]
    extractAndArgs (List (StringLit "and":args')) = args'
    extractAndArgs _ = []
simplify e = e
