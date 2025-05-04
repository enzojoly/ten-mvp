{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-} -- To specify type params like 'Eval

module Main (main) where

import Ten.MonadArchitecture
import Control.Monad.Reader (ask, local)
import Control.Monad.State (get, put, modify)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Singletons (SingI(..)) -- To bring SPhase and SPrivilegeTier into scope
import qualified Data.Text as T
import qualified Data.Set as Set

-- Example TenM action in the Eval phase with Daemon privileges
exampleEvalAction :: TenM 'Eval 'Daemon String
exampleEvalAction = do
    liftIO $ putStrLn "Starting Eval Action..."
    env <- ask
    liftIO $ putStrLn $ "Current workDir: " ++ workDir env
    liftIO $ putStrLn $ "Current privilege: " ++ show (currentPrivilegeTier env)

    -- Modify state
    modify (\s -> s { recursionDepth = recursionDepth s + 1 })
    st <- get
    liftIO $ putStrLn $ "Current recursion depth: " ++ show (recursionDepth st)

    -- Modify state back
    put st { recursionDepth = 0 }
    newSt <- get
    liftIO $ putStrLn $ "Reset recursion depth: " ++ show (recursionDepth newSt)

    -- Add a proof (specific to 'Eval phase)
    modify (\s -> s { buildProofs = TypeProof : buildProofs s })

    -- Example of using local
    res <- local (\e -> e { verbosity = verbosity e + 5 }) $ do
        env' <- ask
        return $ "Verbosity inside local: " ++ show (verbosity env')
    liftIO $ putStrLn res

    -- Example error handling
    -- Now GHC will infer "This is a test error" as Text because EvalError expects Text
    catchError (throwError (EvalError "This is a test error")) $ \err -> do
        liftIO $ putStrLn $ "Caught error: " ++ show err
        return "Recovered from error"

    -- Uncomment to see an uncaught error propagate
    -- throwError (InternalError "Something went wrong")

    liftIO $ putStrLn "... Eval Action Finished"
    return "Eval Action Succeeded"

main :: IO ()
main = do
    putStrLn "--- Running Ten MVP ---"
    let initialEnvDaemon = initialEnv Daemon
    let initialStEval = initialEvalState

    -- Run the example Eval action
    putStrLn "\n--- Running Eval Action ---"
    resultEval <- runTen initialEnvDaemon initialStEval exampleEvalAction
    case resultEval of
        Left err -> putStrLn $ "Eval Failed: " ++ show err
        Right (value, finalState) -> do
            putStrLn $ "Eval Succeeded: " ++ value
            putStrLn $ "Final Eval State: " ++ show finalState

    -- Example of running a Build action (trivial example)
    putStrLn "\n--- Running Build Action ---"
    let initialEnvBuilder = initialEnv Builder -- Use Builder privilege for build
    let initialStBuild = initialBuildState
    let exampleBuildAction :: TenM 'Build 'Builder ()
        exampleBuildAction = do
             liftIO $ putStrLn "Inside Build Action..."
             env <- ask
             liftIO $ putStrLn $ "Build privilege: " ++ show (currentPrivilegeTier env)
             -- Now GHC will infer "fake-hash" and "out" as Text because StorePath expects Text
             modify (\s -> s { buildOutputs = Set.singleton (StorePath "fake-hash" "out") })
             liftIO $ putStrLn "Added dummy build output."

    resultBuild <- runTen initialEnvBuilder initialStBuild exampleBuildAction
    case resultBuild of
        Left err -> putStrLn $ "Build Failed: " ++ show err
        Right (value, finalState) -> do
            putStrLn "Build Succeeded."
            putStrLn $ "Final Build State: " ++ show finalState

    putStrLn "\n--- Ten MVP Finished ---"
