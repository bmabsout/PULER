{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}


module TypeSystem (module TypeSystem) where

import qualified Data.List.NonEmpty as N
import Data.Functor.Foldable
import Data.Foldable
import Data.Functor.Foldable.TH
import Text.Show.Deriving
import Data.Eq.Deriving
import Data.Bifunctor.TH
import Control.Monad
-- import Control.Monad.State
import Data.Map as M
import Data.Set as S
import Data.Char
import Polysemy
import Polysemy.State
import DisjointSets as DSets hiding (find)
import Core
import Control.Comonad.Trans.Env
import Control.Comonad
import Data.Functor.Compose
import Control.Comonad.Cofree
import Data.Monoid
import Debug.Trace
import Data.Functor.Const
import qualified Data.Map.Append as M
import qualified Data.Text.Prettyprint.Doc as Pretty
-- import Control.Comonad.Trans.Cofree (CofreeF(..))
import Data.Void

import BuiltIns

data Itypes base
  = Iunif Name
  | IbaseType base
  | Iarrow (Itypes base) (Itypes base)
  | Imismatch [Itypes base]
  deriving (Show, Eq, Ord, Functor)

makeBaseFunctor ''Itypes

type INamedTypes = Itypes BaseType
type Knowledge = M.Map INamedTypes INamedTypes
type VarMap = M.Map Var INamedTypes
type HasFreshs r = Member (State [Name]) r
type HasVarMap r = Member (State VarMap) r
type HasConstraints r = Member (State Constraints) r
type TypeContext r = (HasFreshs r, HasConstraints r, HasVarMap r)
type AnnotatedTree = Cofree ExprF INamedTypes
type Constraints = DisjointSets INamedTypes

nubQuick :: Ord a => [a] -> [a]
nubQuick = S.fromList &. S.toList

instance Ord base => Semigroup (Itypes base) where
  -- this is actually a join semilattice
  a <> b | a == b = a
  Imismatch as <> Imismatch bs = Imismatch (nubQuick (as ++ bs))
  Iarrow a1 b1 <> Iarrow a2 b2 = Iarrow (a1 <> a2) (b1 <> b2)
  a@(Iunif _) <> (Iunif _) = a
  (Iunif a) <> b = b
  a <> (Iunif b) = a
  (Imismatch l) <> x = if x `elem` l then x else Imismatch (nubQuick $ x:l)
  x <> (Imismatch l) = if x `elem` l then x else Imismatch (nubQuick $ x:l)
  a <> b = Imismatch [a, b]

unif s = Iunif (N.fromList s)

toNonEmtpy :: Pretty.Pretty a => a -> N.NonEmpty Char
toNonEmtpy = Pretty.pretty &. show &. N.fromList

fromStdType :: Types BuiltIns.StdType -> INamedTypes
fromStdType = fmap BaseType &. toInferrable

baseType =  Tbase &. fromStdType
unitType = baseType StdUnit
boolType = baseType StdBool
intType = baseType StdInt
stringType = baseType StdString


unify :: HasConstraints r => INamedTypes -> INamedTypes -> Sem r INamedTypes
unify arr1@(Iarrow a1 b1) arr2@(Iarrow a2 b2) = do
  joined <- Iarrow <$> unify a1 a2 <*> unify b1 b2
  modify (DSets.joinElems arr1 joined)
  modify (DSets.joinElems joined arr2)
  pure joined
unify a b = do
  let joined = a <> b
  modify (DSets.joinElems a joined)
  modify (DSets.joinElems joined b)
  pure joined

unifyM a b = join $ unify <$> a <*> b

getOrFresh :: TypeContext r => Var -> Sem r INamedTypes
getOrFresh var = do
  varMap <- get
  case varMap M.!? var of
    Just typeOfVar -> pure typeOfVar
    Nothing -> do
      typeOfVar <- fresh
      modify (M.insert var typeOfVar)
      pure typeOfVar

typeOfLam (Lambda args body) = do
  argTypes <- sequence (args &> getOrFresh)
  pure $ foldr1 Iarrow $ argTypes <> (body N.:| [])

typeOf :: TypeContext r => ExprF INamedTypes -> Sem r INamedTypes
typeOf (ElitF (Lint _)) = pure intType
typeOf (ElitF (Lbool _)) = pure boolType
typeOf (ElitF (Lstring _)) = pure stringType
typeOf (ElitF Lunit) = pure unitType
typeOf (EvarF var) = getOrFresh var
typeOf (EifF (If shouldBeBool thenThis elseThis)) = do
  unify shouldBeBool boolType
  unify thenThis elseThis
  pure thenThis
typeOf (EappF (App xy x)) = do
  result <- fresh
  unify xy (Iarrow x result)
  pure result
typeOf (ElambdaF lambda) = typeOfLam lambda
typeOf (EfixF (Fixer f lambda)) = unifyM (getOrFresh f) (typeOfLam lambda)
typeOf (EdecsF (Decs decs)) =
  decs & mapM_ decUnify >> pure unitType
typeOf (EletF (Let (Decs decs) expr)) = mapM_ decUnify decs >> pure expr
typeOf (EannotationF (Annotated annoType annoedType)) = unify annoType (toInferrable annoedType)

decUnify (Dec maybeAnnoed x) = do
      x' <- getOrFresh (getA maybeAnnoed)
      unify x x'
      case maybeAnnoed of
        Annoed (Annotated _ anno) -> unify (toInferrable anno) x
        NotAnnoed _ -> pure x


bubbleUpConstraints :: TypeContext r => Expr -> Sem r AnnotatedTree
bubbleUpConstraints = cataM alg'
  where
    alg' :: TypeContext r
         => ExprF AnnotatedTree
         -> Sem r (Cofree ExprF INamedTypes)
    alg' t = (:<) <$> (t &> extract & typeOf) <*> pure t 

annotate :: Expr -> Sem '[State [Name], State VarMap, State Constraints] AnnotatedTree 
annotate expr = do
  annotatedTree <- bubbleUpConstraints expr
  knowledge <- propagateKnowledge
  pure $ fillTypes knowledge annotatedTree

data InferContext = InferContext [Name] VarMap Constraints


initVarMap :: Map Var INamedTypes
initVarMap = builtInMap &> typeOfBuiltIn &. fromStdType

initContext :: InferContext
initContext = InferContext unificationVars initVarMap mempty

infer :: Expr -> (InferContext, AnnotatedTree)
infer = inferWithContext &. runState initContext &. run 

inferWithContext :: Expr -> Sem '[State InferContext] AnnotatedTree
inferWithContext expr = do
  InferContext freshs varMap constraints <- get
  let (newConstraints, (newVarMap, (newFreshs, inferred))) =
        annotate expr
        & runState freshs
        & runState varMap
        & runState constraints
        & run
  put (InferContext newFreshs newVarMap newConstraints)
  pure inferred


fullInfer :: AnnotatedTree -> Either String (Cofree ExprF (Types BaseType))
fullInfer = fmap fullTypes &. sequence

unificationVars :: [Name]
unificationVars = fmap N.fromList $ [1..] >>= flip replicateM ['a'..'z']


fresh :: (HasFreshs r, HasConstraints r) => Sem r INamedTypes
fresh = do
   freshs <- get
   put (tail freshs)
   let freshType = (Iunif $ head freshs)
   modify (DSets.insert freshType)
   pure freshType

type a ~> b = forall x. a x -> b x -- Natural Transformation

toInferrable :: Types ~> Itypes
toInferrable = hoist $ \case TarrowF a b -> IarrowF a b
                             TbaseF n -> IbaseTypeF n

-- fromInferred :: Itypes base -> Either (Types base) String
fullTypes :: (Pretty.Pretty base) => Itypes base -> Either String (Types base)
fullTypes = cataM \case 
                     IarrowF a b -> Right $ Tarrow a b
                     IbaseTypeF n -> Right $ Tbase n
                     IunifF x -> Left $ (Pretty.pretty x & show) ++ ": ununified"
                     ImismatchF x -> Left $ (Pretty.pretty x & show) ++ ": Mismatching"

updateUnifs :: Knowledge -> INamedTypes -> INamedTypes
updateUnifs knowledge = cata \case
    IunifF x -> knowledge M.! (Iunif x)
    x -> Core.embed x

occursCheck :: Set INamedTypes -> INamedTypes -> Bool
occursCheck n = cata \case
  IunifF x -> S.member (Iunif x) n
  x -> or x

gatherKnowledge :: (HasConstraints r) => Sem r Knowledge
gatherKnowledge =
  get >>= 
    DSets.elems &. S.toList
        &.> (\set -> do
                let setElems = S.toList set
                joined <- foldr1 unifyM (setElems &> pure)
                if occursCheck (S.delete joined set) joined
                then error "occurs check failed"
                else pure $ setElems &> (\e -> M.singleton e joined) & mconcat
           )
        &. sequence &.> mconcat &.> (\m -> m &> updateUnifs m)

isFull :: INamedTypes -> Bool
isFull = cata \case
  (IunifF _) -> False
  x -> and x

fillTypes :: Functor f => Knowledge -> f INamedTypes -> f INamedTypes
fillTypes knowledge = fmap (updateUnifs knowledge)

allSolved :: Knowledge -> Bool
allSolved constraints = all isFull (S.fromList $ M.elems constraints)

propagateKnowledge :: (HasVarMap r, HasConstraints r) => Sem r Knowledge
propagateKnowledge = do
  knowledge <- reachFixPoint gatherKnowledge
  updateVarMap knowledge
  pure knowledge

updateVarMap :: HasVarMap r => Knowledge -> Sem r ()
updateVarMap knowledge = modify (fillTypes knowledge :: VarMap -> VarMap)

reachFixPoint :: forall m a. (Eq a, Monad m) => m a -> m a
reachFixPoint m = m >>= helper
  where helper :: a -> m a
        helper a = m >>= \b ->
          if a == b then pure b else helper b