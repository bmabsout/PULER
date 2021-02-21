{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingVia #-}

module Renamer where

import Core
import qualified Data.Map as M
import qualified Data.Map.Append as M
import Data.List
import Data.Word
import qualified Data.List.NonEmpty as N
import Data.Bifunctor
import Polysemy.State
import Data.Coerce
import Data.Semigroup
import PrettyStuff
import Debug.Trace
import Polysemy
import PrettyStuff
import qualified Control.Monad.State.Class as MST
import qualified Control.Monad.State as MST
import qualified BuiltIns

data Scoper m a where
  Push :: Var -> Scoper m Var
  Pop :: Var -> Scoper m () 
  GetVar :: Var -> Scoper m Var
makeSem ''Scoper

type Scoped = Sem '[Scoper]

data ScopeError = NotInScope Var

type Renamer = M.Map Var Scope

initRenameState :: Renamer
initRenameState =
  builtInMap @BuiltIns.StdFunction
  & M.keys &> (\k -> insertToScope k mempty)
  & mconcat

rename :: Expr -> (Renamer, Expr)
rename = renameWithState &. runState initRenameState &. run

renameWithState :: Expr -> Sem '[State Renamer] (Expr)
renameWithState = cata renameCoalg &. scoperReInterpreter


scoperReInterpreter :: Sem '[Scoper] a -> Sem '[State Renamer] a
scoperReInterpreter = reinterpret \case
  Push var   -> let cleaned = clean var
    in modify (insertToScope cleaned) >> scoperReInterpreter (getVar cleaned)
  Pop var    -> let cleaned = clean var
    in modify (removeFromScope cleaned)
  GetVar var -> let cleaned = clean var
    in get &> varFromScope cleaned

clean :: Var -> Var
clean (Var s) = Var $ N.fromList $ takeWhile (/= '#') (N.toList s)

renameCoalg :: ExprF (Scoped Expr) -> Scoped Expr
renameCoalg (EvarF var) = Evar <$> getVar var
renameCoalg (ElambdaF lambda) =
  Elambda <$> sequenceLambda lambda
renameCoalg (EfixF (Fixer f lambda)) = do
  scopedF <- push f
  scopedLambda <- sequenceLambda lambda
  pop f
  return $ Efix $ Fixer scopedF scopedLambda
renameCoalg (EdecsF decs) = Edecs <$> sequenceDecs decs
renameCoalg (EletF (Let decs bodyM)) = do
  (Decs scopedDecs) <- sequenceDecs decs
  body <- bodyM
  mapM_ (\(Dec annoed _) -> pop (getA annoed)) scopedDecs
  return $ Elet (Let (Decs scopedDecs) body)
renameCoalg expr = Core.embed <$> sequence expr

sequenceLambda :: Traversable f 
               => Lambda (f Var) (Scoped expr)
               -> Scoped (Lambda (f Var) expr)
sequenceLambda (Lambda args body) = do
  scopedArgs <- traverse push args
  scopedBody <- body
  mapM_ pop scopedArgs
  return $ Lambda scopedArgs scopedBody

sequenceDecs :: Decs (Scoped expr) -> Scoped (Decs expr)
sequenceDecs (Decs decs) =
  let sequenceDec :: Dec (PossibleAnno Var b) (Scoped c)
                   -> Scoped (Dec (PossibleAnno Var b) c)
      sequenceDec (Dec annoed body) = do
        scopedBody <- body
        scopedAnnoed <- push (getA annoed) &> setA annoed
        return $ Dec scopedAnnoed scopedBody
  in Decs <$> traverse sequenceDec decs


newtype Shadowing = Shadowing Word
    deriving (Eq, Num, Ord, Show)

instance Pretty Shadowing where
    pretty 0 = ""
    pretty (Shadowing n) = "#" <> pretty n

data Scope = Scope {numShadowing :: Shadowing, stack :: [Shadowing]}

instance Pretty Scope where
  pretty scope = "i:" <> pretty (numShadowing scope) <> "," <+> "s:" <> pretty (stack scope)


insertToScope :: Var -> Renamer -> Renamer
insertToScope =
  M.alter $ Just . \case
    Just (Scope g l) -> Scope (g + 1) (g + 1 : l)
    Nothing -> Scope 0 $ [0]

removeFromScope :: Var -> Renamer -> Renamer
removeFromScope = M.update \(Scope g l) -> Just (Scope g (tail l))

varFromScope :: Var -> Renamer -> Var
varFromScope var map = 
  Var $ N.fromList $ show $ pretty var
       <> (pretty $ head $ stack $ map M.! var)
