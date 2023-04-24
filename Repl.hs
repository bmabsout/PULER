{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Repl where

import Core
import qualified Parser
import qualified Renamer
import qualified Evaluator
import qualified TypeSystem
import qualified Compiler

import Control.Monad.Trans
import Data.List (isPrefixOf)
import System.Console.Repline
import Control.Monad
import Polysemy
import Polysemy.Output
import Polysemy.Trace
import qualified Polysemy.State as P
import Control.Monad.Trans.State.Strict (evalStateT, StateT)
import Control.Monad.State.Class
import qualified Data.Map as M
import Distribution.Compat.Lens (_1)
import Prettyprinter (pretty)

type Repl a = HaskelineT (StateT Compiler.Context IO) a


-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = do
    contexed <- effectsToMtl $ Compiler.withContext input
    case contexed of
        Left err -> liftIO $ putStrLn err
        Right expr ->
            liftIO $ Compiler.pprint expr

-- Tab Completion: return a completion for partial words entered
completer :: (Monad m, MonadState Compiler.Context m) => WordCompleter m
completer n = do
  context <- get
  return $ filter (isPrefixOf n) (Compiler.renamer context & M.keys &> pretty &> show)

-- Commands
help :: String -> Repl ()
help _ = do
    m <- lift get
    liftIO $ Compiler.pprint m

typeof :: String -> Repl ()
typeof statement = do
    (Right typ) <- Compiler.typeOf statement & embedState & runM
    liftIO $ Compiler.pprint typ


opts :: [(String, String -> Repl ())]
opts =
  [ ("help", help)
  , ("type", typeof)
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to PULER!"


repl :: Compiler.Context -> IO ()
repl = evalRepl (const $ pure "λ> ") cmd opts (Just ':') Nothing (Word completer) ini (pure Exit) & evalStateT

effectsToMtl :: (MonadIO m, MonadState s m) => Sem [P.State s, Trace, Embed m] a -> m a
effectsToMtl = embedState &. embedTrace &. runM

embedTrace :: (MonadIO m, Member (Embed m) r) => Sem (Trace:r) a -> Sem r a
embedTrace = interpret \case
  Trace s -> Polysemy.embed $ liftIO $ putStrLn s

embedState :: (MonadState state m, Member (Embed m) r) => Sem (P.State state:r) a -> Sem r a
embedState = interpret \case
  P.Get -> Polysemy.embed get
  P.Put s -> Polysemy.embed (put s)
-- state \initState ->
    -- P.runState initState sem & _
-- parse :: Member Console r => String -> Repl r ()
-- parse [] = output "nothing to parse"
-- parse s = (Parser.parseHelper @Expression s)
--           & \case Left x -> output x
--                   Right x -> output $ show $  PrettyStuff.pretty x