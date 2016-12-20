{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Horname.Internal.SMT where

import           Data.Data
import           Data.List (foldl', find)
import qualified Data.List as List
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
insertBindings m bindings = foldl' (insertBinding m) m bindings

insertBinding :: Map Text SExpr -> Map Text SExpr -> SExpr -> Map Text SExpr
insertBinding oldMap m (List [StringLit key, val]) = Map.insert key (inlineLets' oldMap val) m
insertBinding _ _ _ = error "Syntax error in let bindings"

-- | This is not correct in the case of quantifiers but ignoring this
-- simplifies the implementation and seems to be enough for z3 and
-- eldarica
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
partitionPosNeg (Neg e) = ([], [e])
partitionPosNeg e = ([e], [])

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

pattern And :: [SExpr] -> SExpr
pattern And args = List (StringLit "and" : args)

pattern Neg :: SExpr -> SExpr
pattern Neg arg = List [StringLit "-", arg]

pattern Or :: [SExpr] -> SExpr
pattern Or args = List (StringLit "or" : args)

pattern BinOp :: Text -> SExpr -> SExpr -> SExpr
pattern BinOp name op1 op2 = List [StringLit name, op1, op2]

-- first pass of simplifications
simplify :: SExpr -> SExpr
-- (* (- 1) x) → x
simplify (BinOp "*" (Neg (IntLit i)) expr) =
  let expr' =
        if i == 1
          then expr
          else BinOp "*" (IntLit i) expr
  in Neg expr'
-- merge nested ands
simplify (And args) = And (andArgs ++ others)
  where
    (ands, others) =
      partition
        (\case
           And args' -> Left args'
           e -> Right e)
        args
    andArgs = concat ands
-- pull out common subexpressions of disjunctions
simplify (Or (arg:args)) =
  if null commonSubExprs
    then Or (arg : args)
    else And
           (commonSubExprs ++
            [Or (map (removeSubExprs commonSubExprs) (arg : args))])
  where
    commonSubExprs =
      filter
        (\arg' -> all (arg' `subsumedBy`) args)
        (case arg of
           And exprs -> exprs
           _ -> [])
    subsumedBy :: SExpr -> SExpr -> Bool
    subsumedBy e (And args') = e `elem` args'
    subsumedBy _ _ = False
    removeSubExprs :: [SExpr] -> SExpr -> SExpr
    removeSubExprs subExprs (And exprs) =
      And (filter (\e -> not (e `elem` subExprs)) exprs)
    removeSubExprs _ e = e
-- Move negative and positive arguments to the same side of a comparison
simplify (BinOp opName arg1 arg2)
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
    sepSubtraction (BinOp "-" arg1 arg2) = [arg1, Neg arg2]
    sepSubtraction e = [e]
-- transform (not (or (not …))) to (and …)
simplify (List [StringLit "not", Or args]) = And (map negateExpr args)
simplify e = e

antiSymmetricOp :: Text -> Bool
antiSymmetricOp n = n `elem` ["<=",">="]
-- second pass of simplifications
simplify' :: SExpr -> SExpr
-- transform two inequalities to an equality
simplify' (And args) = And (other ++ mergeInequalities inequality)
  where
    (inequality, other) =
      List.partition
        (\case
           List [StringLit op, _, _] -> antiSymmetricOp op
           _ -> False)
        args
    mergeInequalities :: [SExpr] -> [SExpr]
    mergeInequalities [] = []
    mergeInequalities (e@(List [StringLit op, expr1, expr2]):rest) =
      let reversedE = List [StringLit op, expr2, expr1]
      in if reversedE `elem` rest
           then BinOp "=" expr1 expr2 :
                mergeInequalities (filter (not . (`elem` [e, reversedE])) rest)
           else e : mergeInequalities rest
    mergeInequalities (e:es) = e : mergeInequalities es
simplify' e = e


negateExpr :: SExpr -> SExpr
negateExpr (List [StringLit "not", expr]) = expr
negateExpr expr = List [StringLit "not", expr]

extractDefinitions :: Map Text [Text] -> [DefineFun] -> [DefineFun]
extractDefinitions decls defs =
  mapMaybe
    (\(name, argNames) ->
       renameDefineFun argNames <$> find ((== name) . funName) defs)
    (Map.toList decls)
