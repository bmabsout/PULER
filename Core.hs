{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
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
import Prettyprinter
import Flow
import BuiltIns
import Control.Monad ((<=<))

type Name = N.NonEmpty Char

newtype Var = Var (N.NonEmpty Char)
    deriving (Eq, Ord)

instance Show Var where
  show (Var x) = N.toList x
instance IsString Var where
  fromString = N.fromList &. Var
instance Pretty Var where
    pretty (Var name) = pretty name


infixr :->
pattern a :-> b = Tarrow a b
data Types types
  = Tarrow (Types types) (Types types)
  | Tbase types
  deriving (Show, Eq, Functor)
deriveEq1 ''Types
makeBaseFunctor ''Types

instance Pretty a => Pretty (Types a) where
    pretty (x :-> y) = pretty x <> " -> " <> pretty y
    pretty (Tbase x) = pretty x

type NamedTypes = Types StdType


data If expr = If expr expr expr
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Pretty expr => Pretty (If expr) where
  pretty (If expr yes no) = align $ vsep ["if " <> pretty expr, "then " <> pretty yes, "else " <> pretty no]


data Lambda args expr = Lambda args expr
  deriving (Show, Eq, Functor, Foldable, Traversable)
deriveBifunctor ''Lambda
deriveShow2 ''Lambda

data App expr = App expr expr
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance (Pretty arg, Pretty expr) => Pretty (Lambda (N.NonEmpty arg) expr) where
  pretty (Lambda args body) = "\\" <> sep (N.toList $ pretty <$> args) <+> "->" <> nest 4 (softline <> pretty body)


data Fixer expr = Fixer Var (Lambda (N.NonEmpty Var) expr)
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance (Pretty expr) => Pretty (Fixer expr) where
  pretty (Fixer f lambda) = "fix " <> pretty f <+> pretty lambda


data Dec var expr = Dec var expr
  deriving (Show, Eq, Functor, Foldable, Traversable)
deriveBifunctor ''Dec
deriveShow2 ''Dec

instance (Pretty expr, Pretty var) => Pretty (Dec var expr) where
  pretty (Dec var expr) = pretty var <> " = " <> pretty expr

instance Pretty expr => Pretty (App expr) where
  pretty (App e1 e2) = "(" <> align (pretty e1 <> nest 2 (softline <> pretty e2)) <> ")"


data AnnotatedFlipped anno expr = Annotated expr anno
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

deriveBifunctor ''AnnotatedFlipped
deriveShow2 ''AnnotatedFlipped

-- trick to let the first arg be the functor acts on
type Annotated expr anno = AnnotatedFlipped anno expr

instance (Pretty expr, Pretty anno) => Pretty (Annotated expr anno) where
  pretty (Annotated a b) = pretty a <> ":" <> pretty b

data PossibleAnno a b
  = Annoed (Annotated a b)
  | NotAnnoed a
  deriving (Eq, Show)
deriveBifunctor ''PossibleAnno

instance (Pretty a, Pretty b) => Pretty (PossibleAnno a b) where
  pretty (Annoed a) = pretty a
  pretty (NotAnnoed b) = pretty b

getA (Annoed (Annotated a _)) = a
getA (NotAnnoed a) = a

setA (Annoed (Annotated a x)) b = Annoed (Annotated b x)
setA (NotAnnoed a) b = NotAnnoed b

newtype Decs expr = Decs (N.NonEmpty (Dec (PossibleAnno Var NamedTypes) expr))
  deriving (Eq, Functor, Foldable, Traversable)

instance (Pretty expr) => Pretty (Decs expr) where
  pretty (Decs decs) = align $ vsep (N.toList (fmap (pretty &. (<> ";")) decs))


data Let expr = Let (Decs expr) expr
    deriving (Eq, Functor, Foldable, Traversable)

instance Pretty expr => Pretty (Let expr) where
  pretty (Let decs expr) = align $ vsep ["let " <> pretty decs, "in " <> pretty expr]


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

instance Pretty Expr where
    pretty (Evar        expr) = pretty expr
    pretty (Eapp        expr) = pretty expr
    pretty (Elet        expr) = pretty expr
    pretty (Eif         expr) = pretty expr
    pretty (Edecs       expr) = pretty expr
    pretty (Elambda     expr) = pretty expr
    pretty (Efix        expr) = pretty expr
    pretty (Elit        expr) = pretty expr
    pretty (Eannotation expr) = pretty expr

instance (Pretty b) => Pretty (ExprF b) where
    pretty (EvarF        expr) = pretty expr
    pretty (EappF        expr) = pretty expr
    pretty (EletF        expr) = pretty expr
    pretty (EifF         expr) = pretty expr
    pretty (EdecsF       expr) = pretty expr
    pretty (ElambdaF     expr) = pretty expr
    pretty (EfixF        expr) = pretty expr
    pretty (ElitF        expr) = pretty expr
    pretty (EannotationF expr) = pretty expr


typeOfBuiltIn :: StdFunction -> Types StdType
typeOfBuiltIn EqInt =  Tbase StdInt :-> Tbase StdInt :-> Tbase StdBool
typeOfBuiltIn Print = Tbase StdString :-> Tbase StdUnit
typeOfBuiltIn Int2Str = Tbase StdInt :-> Tbase StdString
typeOfBuiltIn f
    | f `elem` [Mul, Add, Sub] = Tbase StdInt :-> Tbase StdInt :-> Tbase StdInt
    | f `elem` [Neg, Pos] = Tbase StdInt :-> Tbase StdInt

builtInMap :: (Enum a, Bounded a, Pretty a) => Map.Map Var a
builtInMap = enumOneToOne (pretty &. show &. fromString)

cataM :: (Monad m, Traversable (Base a), Recursive a)
      => (Base a b -> m b) -> a -> m b
cataM f = f <=< cata (traverse (>>= f))

app x y = Eapp (App x y)
var x = Evar (fromString x)
