{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Repl where

import Core
import qualified Parser
import qualified Renamer
import qualified Evaluator
import qualified PrettyStuff
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

type Repl a = HaskelineT (StateT Compiler.Context IO) a


-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = do
    contexed <- effectsToMtl $ Compiler.withContext input
    case contexed of
        Left err -> liftIO $ putStrLn err
        Right expr ->
            liftIO $ PrettyStuff.print expr

-- Tab Completion: return a completion for partial words entered
completer :: (Monad m, MonadState Compiler.Context m) => WordCompleter m
completer n = do
  context <- get
  return $ filter (isPrefixOf n) (Compiler.renamer context & M.keys &> PrettyStuff.pretty &> show)

-- Commands
help :: [String] -> Repl ()
help args = do
    m <- lift $ get
    liftIO $ PrettyStuff.print m

typeof :: [String] -> Repl ()
typeof args = do
    let statement = unwords args
    (Right typ) <- Compiler.typeOf statement & embedState & runM
    liftIO $ PrettyStuff.print typ


opts :: [(String, [String] -> Repl ())]
opts =
  [ ("help", help)
  , ("type", typeof)
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to DRAT!"

repl :: Compiler.Context -> IO ()
repl ctxt = evalRepl (pure "λ> ") cmd (opts) (Just ':') (Word0 completer) ini
       & flip evalStateT ctxt


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