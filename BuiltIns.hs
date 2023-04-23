{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
module BuiltIns where

import Data.Void
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec as M
import Data.Text.Prettyprint.Doc
import Data.Map as Map
import Parsable
import Flow
import GHC.Exts (IsString(fromString))

newtype ShowPretty a = ShowPretty a
instance Show a => Pretty (ShowPretty a) where
    pretty (ShowPretty a) = fromString $ show a

data StdFunction
  = Mul | Add | Sub | Neg | Pos | EqInt | Print |Int2Str
  deriving (Eq, Ord, Enum, Bounded, Show)
  deriving Pretty via (ShowPretty StdFunction)

data StdType
  = StdInt | StdBool | StdString | StdUnit
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Pretty StdType where
    pretty StdInt = "Int"
    pretty StdBool = "Bool"
    pretty StdString = "String"
    pretty StdUnit = "{}"

builtIns :: Map.Map String StdFunction
builtIns = enumOneToOne show

allInhabitants :: (Enum a, Bounded a) => [a]
allInhabitants = enumFrom minBound

enumOneToOne :: (Enum a, Bounded a, Ord b) => (a -> b) -> Map.Map b a
enumOneToOne f = allInhabitants
  &> (\x -> (f x, x))
  & Map.fromList


data Lit = Lint Int | Lbool Bool | Lstring String | Lunit
    deriving (Show, Eq)