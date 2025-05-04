{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-} -- To specify type params like 'Eval

module Main (main) where

import Ten.MonadArchitecture
import Control.Monad.Reader (ask, local)
import Control.Monad.State.Strict (get, put, modify) -- Use strict state
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Singletons (SingI(..), sing) -- Use 'sing' to get singleton values if needed
import qualified Data.Text as T
import qualified Data.Set as Set

-- Example TenM action in the Eval phase with Daemon privileges
exampleEvalAction :: TenM 'Eval 'Daemon String
exampleEvalAction = do
    liftIO $ putStrLn "Starting Eval Action..."
    env <- ask
    st <- get
    liftIO $ putStrLn $ "Current Phase (Runtime): " ++ show (currentPhase st)
    liftIO $ putStrLn $ "Current workDir: " ++ workDir env
    liftIO $ putStrLn $ "Current privilege: " ++ show (currentPrivilegeTier env)

    -- Modify state
    modify (\s -> s { recursionDepth = recursionDepth s + 1 })
    st' <- get
    liftIO $ putStrLn $ "Current recursion depth: " ++ show (recursionDepth st')

    -- Modify state back (using put)
    put st' { recursionDepth = 0 }
    newSt <- get
    liftIO $ putStrLn $ "Reset recursion depth: " ++ show (recursionDepth newSt)

    -- Add a proof (specific to 'Eval phase)
    modify (\s -> s { buildProofs = TypeProof : buildProofs s })
    stProof <- get
    liftIO $ putStrLn $ "Current Proofs: " ++ show (buildProofs stProof)


    -- Example of using local
    res <- local (\e -> e { verbosity = verbosity e + 5 }) $ do
        env' <- ask
        liftIO $ putStrLn $ "Verbosity inside local: " ++ show (verbosity env')
        return $ "Result from local block"
    liftIO $ putStrLn $ "Local result: " ++ res

    -- Example error handling
    catchError (throwError (EvalError "This is a test error")) $ \err -> do
        liftIO $ putStrLn $ "[Caught Error]: " ++ show err
        return "Recovered from error"

    -- Uncomment to see an uncaught error propagate
    -- liftIO $ putStrLn "About to throw uncaught error..."
    -- throwError (InternalError "Something went wrong uncaught")

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
        Left err -> putStrLn $ "[FAIL] Eval Failed: " ++ show err
        Right (value, finalState) -> do
            putStrLn $ "[OK] Eval Succeeded: " ++ value
            putStrLn $ "[OK] Final Eval State: " ++ show finalState

    -- Example of running a Build action (trivial example)
    putStrLn "\n--- Running Build Action ---"
    let initialEnvBuilder = initialEnv Builder -- Use Builder privilege for build
    let initialStBuild = initialBuildState
    let exampleBuildAction :: TenM 'Build 'Builder ()
        exampleBuildAction = do
             liftIO $ putStrLn "Inside Build Action..."
             env <- ask
             st <- get
             liftIO $ putStrLn $ "Current Phase (Runtime): " ++ show (currentPhase st)
             liftIO $ putStrLn $ "Build privilege: " ++ show (currentPrivilegeTier env)
             -- Add a dummy build output (StorePath expects Text)
             let dummyOutput = StorePath "fake-hash-123" "output-file"
             modify (\s -> s { buildOutputs = Set.insert dummyOutput (buildOutputs s) })
             liftIO $ putStrLn $ "Added build output: " ++ show dummyOutput
             st' <- get
             liftIO $ putStrLn $ "Current outputs: " ++ show (buildOutputs st')

    resultBuild <- runTen initialEnvBuilder initialStBuild exampleBuildAction
    case resultBuild of
        Left err -> putStrLn $ "[FAIL] Build Failed: " ++ show err
        Right ((), finalState) -> do -- Match on ((), finalState)
            putStrLn "[OK] Build Succeeded."
            putStrLn $ "[OK] Final Build State: " ++ show finalState

    putStrLn "\n--- Ten MVP Finished ---"
