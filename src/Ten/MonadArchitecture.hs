{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-} -- Needed for singleton definitions
{-# LANGUAGE TemplateHaskell #-} -- Often used with singletons, but can define manually
{-# LANGUAGE RankNTypes #-} -- For the runner helper
{-# LANGUAGE ScopedTypeVariables #-} -- For the runner helper
{-# LANGUAGE TypeApplications #-} -- For the runner helper
{-# LANGUAGE DeriveGeneric #-} -- For potential future instances (e.g., Binary)
{-# LANGUAGE DerivingStrategies #-} -- For deriving Show/Eq on GADTs cleanly

-- | Defines the core TenM monad and its instances,
--   along with minimal supporting types required for its definition.
module Ten.MonadArchitecture (
    -- * Core Monad
    TenM(..),
    runTenM, -- Export the primitive runner if needed elsewhere

    -- * Runner Helper
    runTenAction,
    KnownPhase(..), -- Export type classes for potential extension
    KnownTier(..),

    -- * Supporting Types (Minimal Definitions)
    Phase(..),
    PrivilegeTier(..),
    SPhase(..),         -- Singleton for Phase
    SPrivilegeTier(..), -- Singleton for PrivilegeTier
    BuildEnv(..),       -- Minimal Environment
    BuildState(..),     -- Minimal GADT State
    BuildError(..),     -- Minimal Error type

    -- * Re-exports for convenience
    module Control.Monad.Reader,
    module Control.Monad.State.Strict,
    module Control.Monad.Except,
    module Control.Monad.IO.Class
) where

import Control.Monad.Reader (ReaderT, runReaderT, ask, local, MonadReader)
import Control.Monad.State.Strict (StateT, runStateT, get, put, modify, MonadState) -- Use strict state
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fail (MonadFail(..)) -- Explicit import for clarity
import Data.Kind (Type)
import qualified Data.Text as T
import GHC.Generics (Generic) -- For potential future instances

-- -----------------------------------------------------------------------------
-- Minimal Supporting Types for TenM Definition
-- -----------------------------------------------------------------------------

-- | Represents the distinct phases of computation at compile time.
data Phase = Eval | Build
    deriving (Show, Eq, Ord, Generic)

-- | Represents the privilege level granted to an action at compile time.
data PrivilegeTier = Daemon | Builder
    deriving (Show, Eq, Ord, Generic)

-- | Singleton type for Phase. GADT ensures constructors match types.
data SPhase (p :: Phase) where
    SEval :: SPhase 'Eval
    SBuild :: SPhase 'Build
deriving instance Show (SPhase p)
deriving instance Eq (SPhase p)


-- | Singleton type for PrivilegeTier. GADT ensures constructors match types.
data SPrivilegeTier (t :: PrivilegeTier) where
    SDaemon :: SPrivilegeTier 'Daemon
    SBuilder :: SPrivilegeTier 'Builder
deriving instance Show (SPrivilegeTier t)
deriving instance Eq (SPrivilegeTier t)


-- | Minimal build environment.
data BuildEnv = BuildEnv {
    envMarker :: T.Text -- A placeholder field
} deriving (Show, Eq, Generic)

-- | Minimal build state, parameterized by phase using a GADT.
data BuildState (p :: Phase) where
    StEval :: { evalMarker :: Int } -> BuildState 'Eval
    StBuild :: { buildMarker :: Bool } -> BuildState 'Build

-- Need Show instances for GADTs if you want to print them
deriving instance Show (BuildState p)
deriving instance Eq (BuildState p)
-- Add Generic instance if needed later, requires StandaloneDeriving
-- deriving instance Generic (BuildState p)

-- | Minimal error type for the monad.
data BuildError
    = BuildFailed T.Text
    | SomeOtherError
    deriving (Show, Eq, Generic)

-- -----------------------------------------------------------------------------
-- The TenM Monad Definition and Instances
-- -----------------------------------------------------------------------------

-- | The core monad for all Ten operations
-- Parameterised by phase and privilege tier with singleton evidence passing
newtype TenM (p :: Phase) (t :: PrivilegeTier) a = TenM
    { -- | The primitive runner requiring explicit singleton evidence.
      runTenM :: SPhase p -> SPrivilegeTier t -> ReaderT BuildEnv (StateT (BuildState p) (ExceptT BuildError IO)) a
    }

-- Standard instances for TenM
instance Functor (TenM p t) where
    fmap f (TenM g) = TenM $ \sp st -> fmap f (g sp st)
    {-# INLINE fmap #-}

instance Applicative (TenM p t) where
    pure x = TenM $ \_ _ -> pure x
    {-# INLINE pure #-}
    (TenM f) <*> (TenM g) = TenM $ \sp st -> f sp st <*> g sp st
    {-# INLINE (<*>) #-}

instance Monad (TenM p t) where
    (TenM m) >>= f = TenM $ \sp st -> do
        a <- m sp st
        let (TenM m') = f a
        m' sp st
    {-# INLINE (>>=) #-}

-- MonadError instance allows throwError and catchError
instance MonadError BuildError (TenM p t) where
    throwError e = TenM $ \_ _ -> throwError e
    {-# INLINE throwError #-}
    catchError (TenM m) h = TenM $ \sp st ->
        catchError (m sp st) (\e -> let (TenM m') = h e in m' sp st)
    {-# INLINE catchError #-}

-- MonadReader instance allows ask and local
instance MonadReader BuildEnv (TenM p t) where
    ask = TenM $ \_ _ -> ask
    {-# INLINE ask #-}
    local f (TenM m) = TenM $ \sp st -> local f (m sp st)
    {-# INLINE local #-}

-- MonadState instance allows get and put
instance MonadState (BuildState p) (TenM p t) where
    get = TenM $ \_ _ -> get
    {-# INLINE get #-}
    put s = TenM $ \_ _ -> put s
    {-# INLINE put #-}

-- MonadIO instance allows liftIO
instance MonadIO (TenM p t) where
    liftIO m = TenM $ \_ _ -> liftIO m
    {-# INLINE liftIO #-}

-- MonadFail instance for pattern matching in do-notation
instance MonadFail (TenM p t) where
    fail msg = TenM $ \_ _ -> throwError $ BuildFailed $ T.pack msg
    {-# INLINE fail #-}

-- -----------------------------------------------------------------------------
-- Runner Helper
-- -----------------------------------------------------------------------------

-- | Typeclass to automatically provide the SPhase singleton.
class KnownPhase (p :: Phase) where
    phaseVal :: SPhase p

instance KnownPhase 'Eval where
    phaseVal = SEval

instance KnownPhase 'Build where
    phaseVal = SBuild

-- | Typeclass to automatically provide the SPrivilegeTier singleton.
class KnownTier (t :: PrivilegeTier) where
    tierVal :: SPrivilegeTier t

instance KnownTier 'Daemon where
    tierVal = SDaemon

instance KnownTier 'Builder where
    tierVal = SBuilder

-- | Runs a TenM action, automatically inferring the phase and tier singletons.
runTenAction :: forall p t a. (KnownPhase p, KnownTier t)
             => BuildEnv          -- ^ Initial environment
             -> BuildState p      -- ^ Initial state (must match phase 'p')
             -> TenM p t a        -- ^ The action to run
             -> IO (Either BuildError (a, BuildState p)) -- ^ Final result or error
runTenAction env initialState action =
    let sp = phaseVal @p
        stier = tierVal @t
        readerT = runTenM action sp stier
        stateT = runReaderT readerT env
        exceptT = runStateT stateT initialState
    in runExceptT exceptT
