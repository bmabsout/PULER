{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module PrettyStuff (module PrettyStuff, module Data.Text.Prettyprint.Doc) where

import Core
import TypeSystem

import Data.List.NonEmpty as N
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Void
import qualified Data.Set as S
import Control.Comonad.Cofree
import Control.Comonad
import Polysemy
import BuiltIns
import Data.Coerce
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import qualified System.IO
import Data.Text.Prettyprint.Doc.Render.Text

instance Pretty Lit where
    pretty (Lint i) = pretty i
    pretty (Lbool b) = pretty b
    pretty (Lstring s) = pretty s
    pretty Lunit = "{}"


instance (Pretty base, Ord base) => Pretty (Itypes base) where
    pretty (Iarrow x@(Iarrow _ _) y) = "(" <> pretty x <> ")" <> "->" <> pretty y
    pretty (Iarrow x y) = pretty x <> "->" <> pretty y
    pretty (IbaseType x) = pretty x 
    pretty (Iunif x) = "?" <> pretty x 
    pretty (Imismatch l) = pretty (S.fromList l)

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
    pretty = pretty . M.toList

instance (Pretty expr, Pretty anno) => Pretty (Annotated expr anno) where
    pretty (Annotated a b) = pretty a <> ":" <> pretty b

instance (Pretty a, Pretty b) => Pretty (PossibleAnno a b) where
    pretty (Annoed a) = pretty a 
    pretty (NotAnnoed b) = pretty b 

instance Pretty Var where
    pretty (Var name) = pretty name

instance Pretty expr => Pretty (App expr) where
    pretty (App e1 e2) = "(" <> align (pretty e1 <> nest 2 (softline <> pretty e2)) <> ")"

instance (Pretty expr, Pretty var) => Pretty (Dec var expr) where
    pretty (Dec var expr) = pretty var <> " = " <> pretty expr

instance (Pretty expr) => Pretty (Decs expr) where
    pretty (Decs decs) = align $ vsep (N.toList (fmap (pretty &. (<> ";")) decs))

instance Pretty expr => Pretty (Let expr) where
    pretty (Let decs expr) = align $ vsep ["let " <> pretty decs, "in " <> pretty expr]

instance Pretty expr => Pretty (If expr) where
    pretty (If expr yes no) = align $ vsep ["if " <> pretty expr, "then " <> pretty yes, "else " <> pretty no]

instance (Pretty arg,Pretty expr) => Pretty (Lambda (N.NonEmpty arg) expr) where
    pretty (Lambda args body) = "\\" <> sep (N.toList $ pretty <$> args) <+>  "->" <> nest 4 (softline <> (pretty body))

instance (Pretty expr) => Pretty (Fixer expr) where
    pretty (Fixer f lambda) = "fix " <> pretty f <+> pretty lambda


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

instance Pretty (Sem r b) where
    pretty = const "<body>"

print :: Pretty a => a -> IO ()
print = pretty &. (<> line) &. layoutSmart (LayoutOptions (AvailablePerLine 40 1)) &. renderIO System.IO.stdout

instance (Pretty (a (Cofree a b)), Pretty b) => Pretty (Cofree a b) where
    pretty (a :< b) = pretty b <> if isBuiltIn then "" else (":" <> pretty a)
        where isBuiltIn = M.member (show $ pretty b) builtIns

instance Pretty InferContext where
  pretty (InferContext unifs varmap constraints) = pretty (varmap, constraints)

-- instance Pretty a => Show a where
    -- show a = show $ pretty a