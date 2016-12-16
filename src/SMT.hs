{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module SMT where

import           Data.Data
import           Data.List (foldl', find)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
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

renameDefineFun :: [Text] -> DefineFun -> DefineFun
renameDefineFun newNames (DefineFun n args retSort expr) =
  DefineFun n renamedArgs retSort (renameInBody varMap expr)
  where
    renamedArgs =
      zipWith (\(Arg _ sort) name -> Arg (VarName name) sort) args newNames
    varMap :: Map Text Text
    varMap =
      Map.fromList $ zip (map (\(Arg (VarName name) _) -> name) args) newNames
    renameInBody :: Map Text Text -> SExpr -> SExpr
    renameInBody m (StringLit s) =
      case Map.lookup s m of
        Just s' -> StringLit s'
        Nothing -> StringLit s
    renameInBody _ (IntLit i) = IntLit i
    renameInBody m (List exprs) = List (map (renameInBody m) exprs)

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

comparisonOps :: [Text]
comparisonOps = ["=", "<", "<=", ">", ">="]

partitionPosNeg :: SExpr -> ([SExpr],[SExpr])
partitionPosNeg (List (StringLit "+":args)) =
  partition
    (\case
       (List [StringLit "-", e]) -> Right e
       e -> Left e)
    args
partitionPosNeg e = ([e],[])

partition               :: (a -> Either b c) -> [a] -> ([b],[c])
partition p xs = foldr (select p) ([],[]) xs

select :: (a -> Either b c) -> a -> ([b], [c]) -> ([b], [c])
select p x (bs,cs) =
  case p x of
    Left b -> (b:bs,cs)
    Right c -> (bs, c:cs)

nonZero :: SExpr -> Bool
nonZero (IntLit 0) = False
nonZero _ = True

sumExprs :: [SExpr] -> SExpr
sumExprs [] = IntLit 0
sumExprs [e] = e
sumExprs args = List (StringLit "+" : args)

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
           List (StringLit "and":args') -> Left args'
           e -> Right e)
        args
    andArgs = concat ands
-- Move negative and positive arguments to the same side of a comparison
simplify (List [StringLit opName, arg1, arg2])
  | opName `elem` comparisonOps =
    case (partitionPosNeg arg1, partitionPosNeg arg2) of
      ((posLeft, negLeft), (posRight, negRight)) ->
        List
          [ StringLit opName
          , sumExprs . filter nonZero $ (posLeft ++ negRight)
          , sumExprs . filter nonZero $ (posRight ++ negLeft)
          ]
-- Transform (+ a (- b c)) to (+ a b (- c))
simplify (List (StringLit "+":args)) =
  List (StringLit "+" : (sepSubtraction =<< args))
  where
    sepSubtraction :: SExpr -> [SExpr]
    sepSubtraction (List [StringLit "-", arg1, arg2]) =
      [arg1, List [StringLit "-", arg2]]
    sepSubtraction e = [e]
simplify e = e

extractDefinitions :: Map Text [Text] -> [DefineFun] -> [DefineFun]
extractDefinitions decls defs =
  mapMaybe
    (\(name, argNames) ->
       renameDefineFun argNames <$> find ((== name) . funName) defs)
    (Map.toList decls)
