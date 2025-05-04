{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-} -- Now needed for runTenAction if inference fails, but often works automatically

module Main (main) where

import Ten.MonadArchitecture
import Control.Monad.Reader (ask, local)
import Control.Monad.State.Strict (get, put, modify) -- Using strict state
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

-- Example TenM action in the Eval phase with Daemon privileges
-- Note: This now uses the simplified BuildEnv and BuildState 'Eval
exampleEvalAction :: TenM 'Eval 'Daemon String
exampleEvalAction = do
    liftIO $ putStrLn "[Eval Action] Starting..."
    env <- ask
    liftIO $ putStrLn $ "[Eval Action] Env marker: " <> T.unpack (envMarker env)

    -- Modify state (using evalMarker)
    modify (\s -> s { evalMarker = evalMarker s + 10 })
    st <- get
    liftIO $ putStrLn $ "[Eval Action] Current eval marker: " ++ show (evalMarker st)

    -- Modify state back using put
    put st { evalMarker = 0 }
    newSt <- get
    liftIO $ putStrLn $ "[Eval Action] Reset eval marker: " ++ show (evalMarker newSt)

    -- Example of using local (modifying envMarker)
    res <- local (\e -> e { envMarker = envMarker e <> " (local)" }) $ do
        env' <- ask
        st' <- get -- Can still access state inside local
        liftIO $ putStrLn $ "[Eval Action] Env marker inside local: " <> T.unpack (envMarker env')
        liftIO $ putStrLn $ "[Eval Action] State marker inside local: " ++ show (evalMarker st')
        return "Value from local block"
    liftIO $ putStrLn $ "[Eval Action] Local result: " <> res

    -- Example error handling (using simplified BuildError)
    catchError (throwError (BuildFailed "This is a deliberate test error")) $ \err -> do
        liftIO $ putStrLn $ "[Eval Action] Caught error: " ++ show err
        return "Recovered successfully from error"

    -- Uncomment to see an uncaught error propagate
    -- liftIO $ putStrLn "[Eval Action] About to throw uncaught error..."
    -- throwError SomeOtherError

    liftIO $ putStrLn "[Eval Action] ...Finished"
    return "Eval Action Completed Successfully"

-- Example TenM action in the Build phase with Builder privileges
-- Note: This now uses the simplified BuildEnv and BuildState 'Build
exampleBuildAction :: TenM 'Build 'Builder ()
exampleBuildAction = do
     liftIO $ putStrLn "[Build Action] Starting..."
     env <- ask
     st <- get
     liftIO $ putStrLn $ "[Build Action] Env marker: " <> T.unpack (envMarker env)
     liftIO $ putStrLn $ "[Build Action] Initial build marker: " ++ show (buildMarker st)

     -- Modify state (toggling buildMarker)
     modify (\s -> s { buildMarker = not (buildMarker s) })
     st' <- get
     liftIO $ putStrLn $ "[Build Action] Toggled build marker: " ++ show (buildMarker st')

     liftIO $ putStrLn "[Build Action] ...Finished"


main :: IO ()
main = do
    putStrLn "--- Running Simplified Ten Monad Actions ---"

    -- Define initial environment and states based on the new structure
    let initialEnvDaemon = BuildEnv { envMarker = "Daemon Env" }
    let initialStEval = StEval { evalMarker = 100 }

    let initialEnvBuilder = BuildEnv { envMarker = "Builder Env" }
    let initialStBuild = StBuild { buildMarker = False }

    -- Run the example Eval action
    putStrLn "\n--- Running Eval Action ---"
    -- runTenAction infers 'Eval and 'Daemon from the type of exampleEvalAction
    resultEval <- runTenAction initialEnvDaemon initialStEval exampleEvalAction
    case resultEval of
        Left err -> putStrLn $ "[FAIL] Eval Failed: " ++ show err
        Right (value, finalState) -> do
            putStrLn $ "[OK] Eval Succeeded: " ++ value
            putStrLn $ "[OK] Final Eval State: " ++ show finalState

    -- Run the example Build action
    putStrLn "\n--- Running Build Action ---"
    -- runTenAction infers 'Build and 'Builder from the type of exampleBuildAction
    resultBuild <- runTenAction initialEnvBuilder initialStBuild exampleBuildAction
    case resultBuild of
        Left err -> putStrLn $ "[FAIL] Build Failed: " ++ show err
        Right ((), finalState) -> do -- Match on ((), finalState) for () return type
            putStrLn "[OK] Build Succeeded."
            putStrLn $ "[OK] Final Build State: " ++ show finalState

    putStrLn "\n--- Ten Monad Actions Finished ---"
