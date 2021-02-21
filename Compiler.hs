{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Compiler where

import Core
import qualified Parser
import qualified Renamer
import qualified Evaluator
import qualified Emitter
import qualified PrettyStuff as Pretty
import PrettyStuff
import qualified TypeSystem
import Data.Void
import qualified Data.List.NonEmpty as N
import Polysemy
import Polysemy.State
import Polysemy.Trace

import Data.Bifunctor
import qualified Emitter

data Context = Context { renamer :: Renamer.Renamer
                       , scope :: Evaluator.Scope
                       , inferContext :: TypeSystem.InferContext
                       }

init = Context Renamer.initRenameState Evaluator.initScope TypeSystem.initContext

instance Pretty Context where
    pretty (Context renamer evalScope typeScope) = pretty renamer <+> pretty evalScope <+> pretty typeScope

parseAndEval s = second Evaluator.evaluate (Edecs <$> Parser.parseDecs s)

stepsBreakdown s = do
    putStrLn "-----INPUT-----"
    putStrLn s
    let possiblyParsed = Edecs <$> Parser.parseDecs s
    case possiblyParsed of
        Left x -> do
            putStrLn "-----PARSE ERROR-----"
            putStrLn x
            pure Nothing
        Right parsed -> do
            putStrLn "-----PARSED-----"
            Pretty.print parsed
            putStrLn "-----RENAMED-----"
            let (renameCtx, renamed) = Renamer.rename parsed
            Pretty.print renamed
            putStrLn "-----INFERRED-----"
            let (inferredCtx, inferredTree) = TypeSystem.infer renamed
            Pretty.print inferredTree
            putStrLn "-----CHECKED-----"
            case TypeSystem.fullInfer inferredTree of
                Left err -> do
                    Pretty.print (err, inferredCtx)
                    pure Nothing
                Right typeChecks -> do
                    Pretty.print "Typechecks!"
                    putStrLn "-----EVALUATED-----"
                    (evaluatorCtx, evaluated) <- Evaluator.evaluate renamed
                    Pretty.print evaluated
                    putStrLn "-----EMITTED-----"
                    let (runtime, emmited) = Emitter.emit typeChecks
                    Pretty.print (emmited)
                    return $ Just (runtime ++ emmited, Context {
                            renamer = renameCtx
                          , scope = evaluatorCtx
                          , inferContext = inferredCtx
                        })


typeOf :: Member (State Context) r => String -> Sem r (Either String TypeSystem.AnnotatedTree)
typeOf str = do
    (Context renameContext _ inferContext) <- get
    return $
        Parser.parseHelper str
            &> Renamer.renameWithState
            &. runState renameContext &. run &. snd
            &. TypeSystem.inferWithContext
            &. runState inferContext &. run &. snd

withContext :: Members [State Context, Trace] r => String -> Sem r (Either String Evaluator.Value)
withContext str = do
    (Context renameContext evalContext inferContext) <- get
    let parsedErr = Parser.parseHelper str &> \case
                        Left dec@(Dec _ _) -> Edecs (Decs (dec N.:| []))
                        Right expr -> expr
    case parsedErr of
        Left err -> return (Left err)
        Right parsed -> do
            let (newRenameContext, renamed) = Renamer.renameWithState parsed & runState renameContext & run
                (newInferContext, inferred) = TypeSystem.inferWithContext renamed & runState inferContext & run
            case TypeSystem.fullInfer inferred of
                Left err -> pure $ Left $ show $ pretty (err, newInferContext)
                Right _ -> do
                    (newEvalContext, evaled) <- Evaluator.evaluateWithScope renamed & runState evalContext
                    put (Context newRenameContext newEvalContext newInferContext)
                    pure (Right evaled)


