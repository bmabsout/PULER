{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Core
    ( module Core
    , module Data.Functor.Foldable
    , module Flow
    , module BuiltIns
    ) where

import qualified Data.List.NonEmpty as N
import qualified Data.Map as Map

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Text.Show.Deriving
import Data.Eq.Deriving
import Algebra.Lattice
import Data.Bifunctor.TH
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Functor.Classes.Generic.Internal
import Data.Void
import GHC.Generics
import GHC.Exts( IsString(..) )
import Data.Text.Prettyprint.Doc
import Flow
import BuiltIns

type Name = N.NonEmpty Char

newtype Var = Var (N.NonEmpty Char)
    deriving (Eq, Ord)

typeOfBuiltIn :: StdFunction -> Types StdType
typeOfBuiltIn f
    | elem f [Mul, Add, Sub] = Tarrow int (Tarrow int int)
    | elem f [Neg, Pos] = Tarrow int int
    | elem f [EqInt] = Tarrow int (Tarrow int (Tbase StdBool))
    | elem f [Print] = Tarrow (Tbase StdString) (Tbase StdUnit)
    | elem f [Int2Str] = Tarrow (Tbase StdInt) (Tbase StdString)
  where int = Tbase StdInt

builtInMap :: (Enum a, Bounded a, Pretty a) => Map.Map Var a
builtInMap = enumOneToOne (pretty &. show &. fromString)

data BaseType = BaseType StdType
  deriving (Eq, Ord)

instance Pretty BaseType where
    pretty (BaseType n) = pretty n

data Types types
  = Tarrow (Types types) (Types types)
  | Tbase types
  deriving (Show, Eq, Functor)
deriveEq1 ''Types

makeBaseFunctor ''Types

toTypeList :: Types types -> N.NonEmpty (Types types)
toTypeList x@(Tbase _) = x N.:| []
toTypeList (Tarrow arg res) = arg N.<| toTypeList res


instance Pretty a => Pretty (Types a) where
    pretty (Tarrow x y) = pretty x <> " -> " <> pretty y 
    pretty (Tbase x) = pretty x 


type NamedTypes = Types BaseType

instance Show Var where
  show (Var x) = N.toList x 

instance IsString Var where
  fromString = N.fromList &. Var

data Dec var expr = Dec var expr
    deriving (Show, Eq, Functor, Foldable, Traversable)
deriveBifunctor ''Dec
deriveShow2 ''Dec

data AnnotatedFlipped anno expr = Annotated expr anno
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
deriveBifunctor ''AnnotatedFlipped
deriveShow2 ''AnnotatedFlipped
-- trick to let the first arg be the functor acts on
type Annotated expr anno = AnnotatedFlipped anno expr  

data PossibleAnno a b = Annoed (Annotated a b)
                      | NotAnnoed a
  deriving (Eq, Show)
deriveBifunctor ''PossibleAnno

getA (Annoed (Annotated a _)) = a
getA (NotAnnoed a) = a

setA (Annoed (Annotated a x)) b = (Annoed (Annotated b x))
setA (NotAnnoed a) b = (NotAnnoed b)

newtype Decs expr = Decs (N.NonEmpty (Dec (PossibleAnno Var NamedTypes) expr))
  deriving (Eq, Functor, Foldable, Traversable)

data Let expr = Let (Decs expr) expr
    deriving (Eq, Functor, Foldable, Traversable)

data If expr = If expr expr expr
    deriving (Show, Eq, Functor, Foldable, Traversable)

data Lambda args expr = Lambda args expr
    deriving (Show, Eq, Functor, Foldable, Traversable)
deriveBifunctor ''Lambda
deriveShow2 ''Lambda

data App expr = App expr expr
    deriving (Show, Eq, Functor, Foldable, Traversable)

data Fixer expr = Fixer Var (Lambda (N.NonEmpty Var) expr)
    deriving (Show, Eq, Functor, Foldable, Traversable)
 

data Expr
  = Evar Var
  | Eapp (App Expr)
  | Elet (Let Expr)
  | Eif (If Expr)
  | Edecs (Decs Expr)
  | Elambda (Lambda (N.NonEmpty Var) Expr)
  | Efix (Fixer Expr)
  | Elit Lit
  | Eannotation (Annotated Expr NamedTypes)
    deriving (Eq, Generic)
makeBaseFunctor ''Expr
-- $(deriveBifunctor ''ExprF)


cataM :: (Monad m, Traversable (Base a), Recursive a)
      => (Base a b -> m b) -> a -> m b
cataM f = (>>= f) . cata (traverse (>>= f))


app x y = Eapp (App x y)
var x = Evar (fromString x)
