{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler where

import Core
import qualified Parser
import qualified Renamer
import qualified Evaluator
import qualified Emitter
import qualified TypeSystem
import Data.Void
import qualified Data.List.NonEmpty as N
import Polysemy
import Polysemy.State
import Polysemy.Trace

import Data.Bifunctor
import Prettyprinter
import System.IO
import Prettyprinter.Render.Text (renderIO)

data Context = Context { renamer :: Renamer.Renamer
                       , scope :: Evaluator.Scope
                       , inferContext :: TypeSystem.InferContext
                       }

instance Pretty Context where
    pretty (Context renamer evalScope typeScope) = pretty renamer <+> pretty evalScope <+> pretty typeScope

init = Context Renamer.initRenameState Evaluator.initScope TypeSystem.initContext

pprint :: Pretty a => a -> IO ()
pprint = pretty &. (<> line) &. layoutSmart (LayoutOptions (AvailablePerLine 40 1)) &. renderIO stdout

-- instance Pretty (Sem r b) where
--     pretty = const "<body>"


parseAndEval s = second Evaluator.evaluate (Edecs <$> Parser.parseDecs s)

stepsBreakdown :: String -> IO (Maybe (String, Context))
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
            pprint parsed
            putStrLn "-----RENAMED-----"
            let (renameCtx, renamed) = Renamer.rename parsed
            pprint renamed
            putStrLn "-----INFERRED-----"
            let (inferredCtx, inferredTree) = TypeSystem.infer renamed
            pprint inferredTree
            putStrLn "-----CHECKED-----"
            case TypeSystem.fullInfer inferredTree of
                Left err -> do
                    pprint (err, inferredCtx)
                    pure Nothing
                Right typeChecks -> do
                    pprint "Typechecks!"
                    putStrLn "-----EVALUATED-----"
                    (evaluatorCtx, evaluated) <- Evaluator.evaluate renamed
                    pprint evaluated
                    putStrLn "-----EMITTED-----"
                    let (runtime, emmited) = Emitter.emit typeChecks
                    pprint emmited
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


