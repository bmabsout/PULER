{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingVia #-}

module Evaluator(module Evaluator) where

import Core
import qualified Data.Map.Strict as M
import Control.Monad
import Data.Maybe
import Data.List.NonEmpty as N
import Data.List
import Data.Bifunctor
import Data.Functor.Foldable.TH
import Data.Function
import Data.Profunctor
import Polysemy
import Polysemy.State
import Polysemy.Trace
import qualified BuiltIns
import Prettyprinter (Pretty(pretty))

type Scope = M.Map Var Value

type Scoped r = Sem (State Scope:r)

type Effects r = (Member Trace r)

newtype LambdaBody = LambdaBody (forall r. Effects r => Scoped r Value)
instance Pretty LambdaBody where
  pretty _ = "<body>"

data Value
  = Vlit Lit
  | Vlambda (Lambda (N.NonEmpty Var) LambdaBody)
  | VDecs (Decs Value)
  | Empty

instance Pretty Value where
    pretty (Vlambda lam) = pretty lam
    pretty (VDecs decs) = pretty decs
    pretty (Vlit lit) = pretty lit
    pretty Empty = "<empty>"


data Blind
  = Bvar Var
  | Bapp (App Blind)
  | Blet (Let Blind)
  | Bif (If Blind)
  | Bdecs (Decs Blind)
  | Blambda (Lambda (N.NonEmpty Var) Expr)
  | Bfix (Fixer Expr)
  | Blit Lit
  | Bannotation (Annotated Blind NamedTypes)

makeBaseFunctor ''Blind

blindAlg :: Expr -> BlindF Expr
blindAlg (Evar x) = BvarF x
blindAlg (Eapp x) = BappF x
blindAlg (Elet x) = BletF x
blindAlg (Eif x) = BifF x
blindAlg (Edecs x) = BdecsF x
blindAlg (Elambda x) = BlambdaF x
blindAlg (Efix x) = BfixF x
blindAlg (Elit x) = BlitF x
blindAlg (Eannotation x) = BannotationF x

forget :: Member (State s) r => Sem r a -> Sem r a
forget m = do
  s <- get
  a <- m
  put s
  return a

local :: Member (State s) r => (s -> s) -> Sem r a -> Sem r a
local modifier m = forget (modify modifier >> m)

evalCoAlg :: Effects r => Base Blind (Scoped r Value)
          -> Scoped r Value
evalCoAlg (BvarF var) =
  get &> (M.!? var) &> fromMaybe (error (show var ++ " is not in scope"))

evalCoAlg (BletF (Let (Decs decsM) scopedM)) = forget $
  sequence_ (decsM &> sequenceDec) >> scopedM

evalCoAlg (BifF (If boolBxprM e1 e2)) = do
  boolBxpr <- boolBxprM
  case boolBxpr of
    Vlit (Lbool b) -> if b then e1 else e2
    _ -> error "not a boolean statement in if"

evalCoAlg (BappF (App appedM appWithM)) = do
  apped <- appedM
  appWith <- appWithM
  case apped of
      Vlambda (Lambda (arg :| rest) (LambdaBody bodyScopeM)) -> do
        let (LambdaBody newLambdaBody) = LambdaBody $ local (M.insert arg appWith) bodyScopeM
        case rest of
              [] -> newLambdaBody
              _  -> pure $ Vlambda $ Lambda (fromList rest) (LambdaBody newLambdaBody)

evalCoAlg (BfixF (Fixer f (Lambda args expr))) =
  let lambdaBodyWithF = LambdaBody $ local (M.insert f (Vlambda lambda)) $ evaluateWithScope expr
      lambda = Lambda args lambdaBodyWithF
  in pure $ Vlambda lambda

evalCoAlg (BlitF lit) = pure $ Vlit lit

evalCoAlg (BlambdaF (Lambda args expr)) = pure $ Vlambda (Lambda args (LambdaBody (evaluateWithScope expr)))

evalCoAlg (BdecsF (Decs decsM)) = do
  decs <- sequence (decsM &> sequenceDec)
  pure $ VDecs $ Decs decs

sequenceDec :: Dec (PossibleAnno Var types) (Scoped r Value)
            -> Scoped r (Dec (PossibleAnno Var types) Value)
sequenceDec (Dec annoed bodyM) = do
  body <- bodyM
  modify (M.insert (getA annoed) body)
  pure $ Dec annoed body

evaluateWithScope :: Effects r => Expr -> Scoped r Value
evaluateWithScope = hylo evalCoAlg blindAlg

funcToLam :: BuiltIns.StdFunction -> Value
funcToLam BuiltIns.Mul = binaryLam Lint (*)
funcToLam BuiltIns.Add = binaryLam Lint (+)
funcToLam BuiltIns.Sub = binaryLam Lint (-)
funcToLam BuiltIns.EqInt = binaryLam Lbool (==)
funcToLam BuiltIns.Pos = unaryLam Lint id
funcToLam BuiltIns.Neg = unaryLam Lint negate
funcToLam BuiltIns.Int2Str = unaryLam Lstring show
funcToLam BuiltIns.Print = Vlambda $ Lambda ("printMe":| []) $ LambdaBody $ get >>= \m ->
  let printMe = m M.! "printMe" & pretty & show
  in trace printMe >> pure (Vlit Lunit)

binaryLam :: (t -> Lit) -> (Int -> Int -> t) -> Value
binaryLam resType f = 
  (LambdaBody do
      m <- get
      pure (deInt (m M.! "a") `f` deInt (m M.! "b") & resType & Vlit)
  ) & Lambda ("a":| ["b"]) & Vlambda

unaryLam :: (t -> Lit) -> (Int -> t) -> Value
unaryLam resType f =
  (LambdaBody do
      m <- get
      pure (deInt (m M.! "c") & f & resType & Vlit)
  ) & Lambda ("c":| []) & Vlambda

deInt (Vlit (Lint v)) = v
-- deString (Vlit (LStr v)) = v

initScope :: Scope
initScope = enumFrom minBound &> (\x ->
  (Var $ N.fromList $ show x, funcToLam x)) & M.fromList


evaluate :: Expr -> IO (Scope, Value)
evaluate expr = runner $ do
  decs <- evaluateWithScope expr
  currMap <- get
  case currMap M.!? Var (fromList "main") of
    Just res -> pure res
    Nothing -> pure decs
 where
  runner :: Sem '[State Scope, Trace, Embed IO] a
                  -> IO (Scope, a)
  runner = runState initScope &. traceToStdout &. runM
