{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Ten.MonadArchitecture (
    -- Core Types mirroring stack components
    Phase(..), -- Original type
    PrivilegeTier(..), -- Original type
    BuildEnv(..),              -- Context for ReaderT
    BuildState(..),            -- State for StateT (depends on 'p')
    BuildError(..),            -- Error type for ExceptT
    Proof(..),                 -- Part of BuildState

    -- The Monad Definition
    TenM(..), runTenMInternal, -- Export the constructor temporarily for runners

    -- Supporting types used in BuildState/BuildEnv
    BuildId(..),
    RunMode(..),
    BuildStrategy(..),
    StorePath(..),
    Derivation(..), -- Simplified for context
    DerivationInput(..),
    DerivationOutput(..),

    -- Singletons Types and Constructors -- <<< CORRECTED SECTION
    SPhase(..),       -- Exports type SPhase and constructors SEval, SBuild
    SPrivilegeTier(..), -- Exports type SPrivilegeTier and constructors SDaemon, SBuilder

    -- Runner Helper (Example)
    runTen,

    -- Initial State/Env Helpers (Example)
    initialEnv, initialEvalState, initialBuildState
) where

import Control.Monad.Reader (MonadReader(..), ReaderT(..)) -- Explicit constructor import
import Control.Monad.State (MonadState(..), StateT(..))   -- Explicit constructor import
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT) -- Explicit constructor and RUNNER import
import Control.Monad.IO.Class (MonadIO(..))
import Data.Kind (Type)
import Data.Text (Text) -- Imported Text type
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Unique (Unique, hashUnique, newUnique) -- Not used in MVP, but kept from original
import Data.Singletons -- Import the core definitions
import Data.Singletons.TH -- Import the Template Haskell functions
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T -- Keep qualified import for potential explicit use
import System.Exit (ExitCode) -- Not directly used here but potentially relevant
import Control.DeepSeq (NFData(..)) -- Import class AND methods (rnf)
import System.IO.Unsafe (unsafePerformIO) -- For default BuildId

-- =========================================================================
-- 1. Parameters for TenM (p and t)
-- =========================================================================

-- | Build phases (Parameter 'p')
data Phase = Eval | Build
    deriving (Show, Eq, Generic, Enum, Bounded)
instance NFData Phase

-- | Privilege tiers (Parameter 't')
data PrivilegeTier = Daemon | Builder
    deriving (Show, Eq, Ord, Generic, Enum, Bounded) -- Added Ord
instance NFData PrivilegeTier

-- Generate singletons for Phase and PrivilegeTier
-- This generates SPhase(SEval, SBuild) and SPrivilegeTier(SDaemon, SBuilder)
$(genSingletons [''Phase, ''PrivilegeTier])

-- =========================================================================
-- 2. Types defining the Context/State/Error for the Stack Layers
-- =========================================================================

-- | Core error types (for ExceptT BuildError)
data BuildError
    = EvalError Text
    | BuildFailed Text
    | StoreError Text
    | PrivilegeError Text
    | InternalError Text
    | PlaceholderError Text
    deriving (Show, Eq, Generic)
instance NFData BuildError

-- | Store path (needed for BuildState)
data StorePath = StorePath { storeHash :: !Text, storeName :: !Text }
    deriving (Show, Eq, Ord, Generic)
instance NFData StorePath

-- | Build identifier type (needed for BuildState)
-- Using Int for simplicity in MVP and testing
newtype BuildId = BuildId Int deriving (Show, Eq, Ord, Generic, Num, NFData)

-- Create a globally unique initial BuildId for convenience
{-# NOINLINE initialBuildId #-}
initialBuildId :: BuildId
initialBuildId = BuildId 0 -- Simpler for MVP

-- | Derivation Input/Output/Definition (needed for BuildState buildChain)
data DerivationInput = DerivationInput { inputPath :: !StorePath, inputName :: !Text }
    deriving (Show, Eq, Ord, Generic)
instance NFData DerivationInput

data DerivationOutput = DerivationOutput { outputName :: !Text, outputPath :: !StorePath }
    deriving (Show, Eq, Ord, Generic)
instance NFData DerivationOutput

data BuildStrategy = ApplicativeStrategy | MonadicStrategy
    deriving (Show, Eq, Generic, Enum, Bounded)
instance NFData BuildStrategy

data Derivation = Derivation
    { derivName :: !Text
    , derivHash :: !Text
    , derivBuilder :: !StorePath
    , derivArgs :: ![Text]
    , derivInputs :: !(Set DerivationInput)
    , derivOutputs :: !(Set DerivationOutput)
    , derivEnv :: !(Map Text Text)
    , derivSystem :: !Text
    , derivStrategy :: !BuildStrategy
    , derivMeta :: !(Map Text Text)
    } deriving (Show, Eq, Generic)
instance NFData Derivation

-- | Proofs (Part of BuildState, parameterized by Phase 'p')
data Proof (p :: Phase) where
    TypeProof         :: Proof 'Eval
    AcyclicProof      :: Proof 'Eval
    EvalCompleteProof :: Proof 'Eval
    InputProof        :: Proof 'Build
    BuildProof        :: Proof 'Build
    OutputProof       :: Proof 'Build

deriving instance Show (Proof p)
deriving instance Eq (Proof p)
-- Cannot derive Generic for GADTs easily, NFData needs manual instance if required
instance NFData (Proof p) where rnf !_ = () -- Simple one, might need refinement

-- | State carried through build operations (for StateT (BuildState p))
data BuildState (p :: Phase) = BuildState
    { buildProofs      :: ![Proof p]
    , buildInputs      :: !(Set StorePath)
    , buildOutputs     :: !(Set StorePath)
    , currentBuildId   :: !BuildId
    , buildChain       :: ![Derivation]
    , recursionDepth   :: !Int
    , currentPhase     :: !Phase -- Runtime phase info
    } deriving (Show, Generic)

-- Need NFData instance for BuildState
instance NFData (BuildState p) where
    rnf (BuildState proofs inputs outputs bid chain depth phase) =
        rnf proofs `seq` rnf inputs `seq` rnf outputs `seq` rnf bid `seq` rnf chain `seq` rnf depth `seq` rnf phase

-- | Running mode (part of BuildEnv)
data RunMode = StandaloneMode | ClientMode | DaemonMode
    deriving (Show, Eq, Generic, Enum, Bounded)
instance NFData RunMode

-- | Environment for build operations (for ReaderT BuildEnv)
-- Corrected version: only one storeLocation field
data BuildEnv = BuildEnv
    { workDir              :: !FilePath
    , storeLocation        :: !FilePath -- Correct: Only one instance
    , verbosity            :: !Int
    , allowedPaths         :: !(Set FilePath)
    , runMode              :: !RunMode
    , nameOfUser           :: !(Maybe Text) -- Expects Text
    , buildStrategy        :: !BuildStrategy
    , maxRecursionDepth    :: !Int
    , maxConcurrentBuilds  :: !(Maybe Int)
    , currentPrivilegeTier :: !PrivilegeTier -- Runtime privilege info
    } deriving (Show, Generic)
instance NFData BuildEnv

-- =========================================================================
-- 3. The TenM Monad Transformer Stack Definition
-- =========================================================================
newtype TenM (p :: Phase) (t :: PrivilegeTier) a = TenM
    { runTenMInternal :: ReaderT BuildEnv (StateT (BuildState p) (ExceptT BuildError IO)) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError BuildError
             , MonadState (BuildState p)
             , MonadReader BuildEnv
             )

instance MonadFail (TenM p t) where
    fail msg = throwError $ InternalError $ T.pack ("MonadFail: " ++ msg) -- Explicit pack needed here

-- =========================================================================
-- Helper function to run
-- =========================================================================

-- Generic runner
runTen :: BuildEnv              -- ^ Initial environment
       -> BuildState p          -- ^ Initial state (phase-specific)
       -> TenM p t a            -- ^ The computation to run
       -> IO (Either BuildError (a, BuildState p))
runTen env state (TenM m) =
    runExceptT (runStateT (runReaderT m env) state) -- Uses runExceptT

-- =========================================================================
-- Initial State/Env Helpers (Example)
-- =========================================================================

initialEnv :: PrivilegeTier -> BuildEnv
initialEnv priv = BuildEnv
    { workDir              = "/tmp/ten-work"
    , storeLocation        = "/ten/store"
    , verbosity            = 1
    , allowedPaths         = Set.singleton "/ten/store"
    , runMode              = DaemonMode -- Example default
      -- "testuser" now interpreted as Text due to OverloadedStrings
    , nameOfUser           = Just "testuser"
    , buildStrategy        = ApplicativeStrategy
    , maxRecursionDepth    = 100
    , maxConcurrentBuilds  = Just 4
    , currentPrivilegeTier = priv
    }

initialEvalState :: BuildState 'Eval
initialEvalState = BuildState
    { buildProofs      = []
    , buildInputs      = Set.empty
    , buildOutputs     = Set.empty
    , currentBuildId   = initialBuildId
    , buildChain       = []
    , recursionDepth   = 0
    , currentPhase     = Eval
    }

initialBuildState :: BuildState 'Build
-- Corrected version: fields not duplicated
initialBuildState = BuildState
    { buildProofs      = []
    , buildInputs      = Set.empty
    , buildOutputs     = Set.empty
    , currentBuildId   = initialBuildId
    , buildChain       = []
    , recursionDepth   = 0
    , currentPhase     = Build
    }
