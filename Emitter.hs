{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Emitter(module Emitter) where

import Core
import qualified Data.List.NonEmpty as N
import Data.List
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable
import Data.Functor.Compose
import Data.Functor.Identity
import Control.Comonad.Trans.Cofree (CofreeF(..))
import Control.Comonad.Cofree hiding ((:<))
import qualified Control.Comonad.Cofree as Cofree
import Data.Functor.Base
import Control.Comonad
import Data.Functor.Foldable.TH
import Data.Set
import Polysemy
import TypeSystem
import PrettyStuff
-- import Data.Text.Prettyprint.Doc.Render.String

data Instruction

-- type CFunction
--     = (Dec Var (Lambda (N.NonEmpty Var) [Instruction]))

runtime :: Doc a
runtime = vsep
    [ "from inspect import signature"
    , ""
    , "def curry(func):"
    , "    num_params = len(signature(func).parameters)"
    , "    def rec(args):"
    , "        if len(args) == num_params:"
    , "            return func(*args)"
    , "        return lambda x: rec(args + [x])"
    , "    return rec([])"
    , ""
    , "def let(inn=None,**kwargs):"
    , "    return inn(**kwargs)"
    , ""
    , "def fix(f):"
    , "    return lambda *args: f(fix(f))(*args)"
    , ""
    , "unit = \"{}\""
    , "Mul = lambda x: lambda y: x * y"
    , "Add = lambda x: lambda y: x + y"
    , "Sub = lambda x: lambda y: x - y"
    , "Neg = lambda x: -x"
    , "Pos = lambda x: x"
    , "EqInt = lambda x: lambda y: x == y"
    , "Int2Str = str"
    , "Print = print"
    , ""
    , ""
    ]

type Typed a = Annotated a NamedTypes
type Env = Map.Map Var NamedTypes

data WithEnv
  = Wvar Var
  | Wapp (App WithEnv)
  | WrealApp (Typed (App WithEnv))
  | Wlet (Let WithEnv)
  | Wif (Typed (If WithEnv))
  | Wfunction (Dec (Typed Var, N.NonEmpty (Typed Var), Env) WithEnv)
  | Wlit Lit
makeBaseFunctor ''WithEnv

toSet :: Ord a => N.NonEmpty a -> Set a
toSet = fromList . N.toList

-- zipArgTypes :: (N.NonEmpty Var) -> NamedTypes -> Env
-- zipArgTypes args t = fromList $ zipWith Annotated (N.toList args) (gatherArgs t)

freeVars :: CofreeF ExprF NamedTypes Env -> Env
freeVars (t :< EvarF var) = Map.singleton var t
freeVars (_ :< ElambdaF (Lambda args expr)) = Map.withoutKeys expr (toSet args)

freeVars (_ :< EfixF (Fixer f (Lambda args expr))) = Map.withoutKeys expr (toSet (N.cons f args))
freeVars (_ :< EdecsF (Decs decs)) = Map.withoutKeys exprs vars
    where (vars, exprs) = freeVarsDecs decs
freeVars (_ :< EletF (Let (Decs decs) expr)) = Map.withoutKeys (expr <> exprs) vars
    where (vars, exprs) = freeVarsDecs decs
freeVars (_ :< x) = Foldable.fold x

freeVarsDecs :: N.NonEmpty (Dec (PossibleAnno Var b) Env)
             -> (Set Var, Env)
freeVarsDecs decs =
    decs &> (\(Dec a expr) -> (getA a, expr))
         & N.unzip & (\(names, exprs) -> (toSet names, Foldable.fold exprs))


addFreeVars :: Cofree ExprF NamedTypes -> Cofree ExprF (NamedTypes, Env)
addFreeVars =
    cata (\cofree@(t :< c) -> (t, cofree &> extract &. snd & freeVars) Cofree.:< c)


recursiveLets :: Cofree ExprF NamedTypes -> Cofree ExprF NamedTypes
recursiveLets = cata $ \case
        (t :< EletF (Let (Decs decs) expr)) ->
            cata (\case
               (NonEmptyF hd Nothing) -> t Cofree.:< EletF (Let (Decs $ hd N.:| []) expr)
               (NonEmptyF hd (Just rest)) -> t Cofree.:< EletF (Let (Decs $ hd N.:| []) rest)
            ) decs
        x -> Core.embed x

-- replaceHash :: Cofree ExprF NamedTypes -> Cofree ExprF NamedTypes
-- replaceHash = hoist $ \case
--     (t :< EvarF (Var x)) -> (t :< EvarF (Var $ x & N.toList & convertToPython & N.fromList))
--   where convertToPython x = x &> (\c -> if c == '#' then '_' else c)


emit :: Cofree ExprF NamedTypes -> (String, String)
emit = recursiveLets
    &. cataM emitF
    &. run
    &. show
    &.> (\c -> if c == '#' then '_' else c)
    &. (show runtime, )

emitF :: CofreeF ExprF NamedTypes (Doc a) -> Sem r (Doc a)
emitF (t :< ElitF (Lint i)) = pure $ pretty i
emitF (t :< ElitF (Lbool b)) = pure $ pretty b
emitF (t :< ElitF (Lstring s)) = pure $ pretty s
emitF (t :< ElitF Lunit) = pure "unit"
emitF (t :< EvarF var) = pure $ pretty var
emitF (t :< EifF (If shouldBeBool thenThis elseThis)) = pure $
    "(" <> line
        <> indent 4 thenThis <> line
    <> ") if (" <> line
        <> indent 4 shouldBeBool <> line
    <> ") else (" <> line
        <> indent 4 elseThis <> line
    <> ")"
emitF (t :< EappF (App xy x)) = pure $ xy <> "(" <> x <> ")"
emitF (t :< ElambdaF (Lambda args expr)) = pure $
    "curry(lambda " <> arguments  <> ":(" <> line <> indent 4 expr <> line <> "))"
    where arguments = punctuate comma (args &> pretty & N.toList) & hsep
emitF (t :< EfixF (Fixer f (Lambda args expr))) = pure $
    "fix(lambda " <> pretty f <> ": lambda " <> arguments <> ":(" <> line <> indent 4 expr <> line <> "))"
    where arguments = punctuate comma (args &> pretty & N.toList) & hsep
emitF (t :< EdecsF (Decs decs)) = pure $ decs &> emitDec & N.toList & vsep
emitF (t :< EletF (Let (Decs decs) expr)) = pure $
    "let(" <> line
    <> indent 2 (decs &> emitDec & N.toList &> (<> ",") & vsep ) <> line
    <> "inn = lambda " <> (decs &> (\(Dec a _) -> pretty $ getA a) & N.toList & punctuate comma & hsep) <> ":(" <> line
    <> indent 2 expr <> line
    <> "))"
emitF (t :< EannotationF (Annotated annoed _)) = pure $ annoed

emitDec (Dec maybeAnnoed x) =
    pretty (getA maybeAnnoed) <> " = " <> x

emitStdFunction :: StdFunction -> [Doc a] -> Doc a
emitStdFunction Mul (a:b:_) = a <> "*" <> b
emitStdFunction Add (a:b:_)= a <> "+" <> b
emitStdFunction Sub (a:b:_)= a <> "-" <> b
emitStdFunction Neg (a:_) = "-" <> a
emitStdFunction Pos (a:_) = a
emitStdFunction EqInt (a:b:_) = a <> "==" <> b
emitStdFunction Int2Str (a:_) = "Int2Str(" <> a <> ")"
emitStdFunction Print (a:_) = "printf(" <> a <> ")"


