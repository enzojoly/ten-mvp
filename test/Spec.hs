{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-} -- For Arbitrary instances
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-} -- Needed for BuildState pattern matching
{-# OPTIONS_GHC -Wno-orphans #-} -- Allow orphan Arbitrary instances for tests

module Main (main) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (Success) -- Avoid name clash if using ExceptT constructors directly
import Test.QuickCheck.Instances.Text () -- Provides Arbitrary for Text
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.QuickCheck.Function (Fun(..), apply) -- For generating functions

import Ten.MonadArchitecture
import Control.Monad.Except (MonadError(throwError)) -- Use explicit throwError
import qualified Data.Text as T
import Control.Applicative (liftA2) -- Useful for Applicative tests

-- Choose specific phase and tier for testing laws.
type TestPhase = 'Eval
type TestTier = 'Daemon
type TestM = TenM TestPhase TestTier

-- Define initial environment and state for running tests
initialTestEnv :: BuildEnv
initialTestEnv = BuildEnv { envMarker = "test-env" }

initialTestState :: BuildState TestPhase
initialTestState = StEval { evalMarker = 0 }

-- Arbitrary Instances for Test Types (Keep these)
instance Arbitrary BuildEnv where
    arbitrary = BuildEnv <$> arbitrary
    shrink (BuildEnv t) = BuildEnv <$> shrink t

instance Arbitrary (BuildState 'Eval) where
    arbitrary = StEval <$> arbitrary
    shrink (StEval i) = StEval <$> shrink i

instance Arbitrary BuildError where
    arbitrary = oneof
        [ BuildFailed <$> arbitrary
        , pure SomeOtherError
        ]
    shrink (BuildFailed t) = BuildFailed <$> shrink t
    shrink SomeOtherError  = []

-- Runner function for tests (Keep this)
runTest :: TestM a -> IO (Either BuildError (a, BuildState TestPhase))
runTest = runTenAction initialTestEnv initialTestState

-- Property Test Helper (Keep this)
assertEqualM :: (Eq a, Eq (BuildState TestPhase), Show a, Show (BuildState TestPhase))
             => TestM a -> TestM a -> Property
assertEqualM action1 action2 = monadicIO $ do
    res1 <- run $ runTest action1
    res2 <- run $ runTest action2
    assert (res1 == res2)

-- Main test entry point for Hspec
main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "TenM ('Eval 'Daemon)" $ do

    describe "Functor Laws" $ do
        -- These should already be correct from the previous step
        prop "Identity (pure): fmap id . pure === pure" $
            \(x :: Int) -> assertEqualM (fmap id (pure x :: TestM Int)) (pure x)

        prop "Identity (error): fmap id . throwError === throwError" $
            \(e :: BuildError) -> assertEqualM (fmap id (throwError e :: TestM Int)) (throwError e)

        prop "Composition (pure): fmap (f . g) . pure === (fmap f . fmap g) . pure" $
            \(Fn (f :: Int -> Int)) (Fn (g :: Bool -> Int)) (x :: Bool) ->
                assertEqualM (fmap (f . g) (pure x :: TestM Bool)) (fmap f (fmap g (pure x)))

        prop "Composition (error): fmap (f . g) . throwError === (fmap f . fmap g) . throwError" $
            \(Fn (f :: Int -> Int)) (Fn (g :: Bool -> Int)) (e :: BuildError) ->
                let action1 = fmap (f . g) (throwError e :: TestM Bool)
                    action2 = fmap f (fmap g (throwError e :: TestM Bool))
                in assertEqualM action1 action2

    describe "Applicative Laws" $ do
        -- These should also be correct
        prop "Identity: pure id <*> v === v (testing with pure)" $
            \(x :: Int) -> let v = pure x :: TestM Int in assertEqualM (pure id <*> v) v

        prop "Identity: pure id <*> v === v (testing with error)" $
            \(e :: BuildError) -> let v = throwError e :: TestM Int in assertEqualM (pure id <*> v) v

        prop "Homomorphism: pure f <*> pure x === pure (f x)" $
            \(Fn (f :: Int -> Bool)) (x :: Int) ->
                assertEqualM (pure f <*> pure x) (pure (f x) :: TestM Bool)

        prop "Interchange: u <*> pure y === pure ($ y) <*> u (testing with u = pure f)" $
             \(Fn (f :: Int -> Bool)) (y :: Int) ->
                 let u = pure f :: TestM (Int -> Bool)
                 in assertEqualM (u <*> pure y) (pure ($ y) <*> u)

        prop "Interchange: u <*> pure y === pure ($ y) <*> u (testing with u = error)" $
             \(e :: BuildError) (y :: Int) ->
                 let u = throwError e :: TestM (Int -> Bool)
                 in assertEqualM (u <*> pure y) (pure ($ y) <*> u)

        prop "Composition (pure): pure (.) <*> pure f <*> pure g <*> pure x === pure f <*> (pure g <*> pure x)" $
             \(Fn (f :: Bool -> Char)) (Fn (g :: Int -> Bool)) (x :: Int) ->
                 let u = pure f :: TestM (Bool -> Char)
                     v = pure g :: TestM (Int -> Bool)
                     w = pure x :: TestM Int
                 in assertEqualM (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))

        prop "Composition (error left): pure (.) <*> error <*> v <*> w === error <*> (v <*> w)" $
             \(Fn (f :: Bool -> Char)) (Fn (g :: Int -> Bool)) (x :: Int) (e :: BuildError) ->
                 let u = throwError e :: TestM (Bool -> Char)
                     v = pure g :: TestM (Int -> Bool)
                     w = pure x :: TestM Int
                 in assertEqualM (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))

        prop "Composition (error middle): pure (.) <*> u <*> error <*> w === u <*> (error <*> w)" $
             \(Fn (f :: Bool -> Char)) (Fn (g :: Int -> Bool)) (x :: Int) (e :: BuildError) ->
                 let u = pure f :: TestM (Bool -> Char)
                     v = throwError e :: TestM (Int -> Bool)
                     w = pure x :: TestM Int
                 in assertEqualM (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))

        prop "Composition (error right): pure (.) <*> u <*> v <*> error === u <*> (v <*> error)" $
             \(Fn (f :: Bool -> Char)) (Fn (g :: Int -> Bool)) (x :: Int) (e :: BuildError) ->
                 let u = pure f :: TestM (Bool -> Char)
                     v = pure g :: TestM (Int -> Bool)
                     w = throwError e :: TestM Int
                 in assertEqualM (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))

        prop "fmap f x === pure f <*> x (pure)" $
             \(Fn (f :: Int -> Bool)) (x :: Int) ->
                 assertEqualM (fmap f (pure x :: TestM Int)) (pure f <*> pure x)

        prop "fmap f x === pure f <*> x (error)" $
             \(Fn (f :: Int -> Bool)) (e :: BuildError) ->
                 assertEqualM (fmap f (throwError e :: TestM Int)) (pure f <*> throwError e)


    describe "Monad Laws" $ do
        -- *** MODIFIED TESTS START HERE ***

        -- Left Identity: pure a >>= k === k a
        -- Test case 1: k returns a pure value
        prop "Left Identity (k returns pure): pure a >>= (pure . k') === pure (k' a)" $
            \(a :: Int) (Fn (k' :: Int -> Bool)) -> -- Generate a pure function k'
                let k :: Int -> TestM Bool
                    k = pure . k' -- Construct the monadic function k inside the test
                in assertEqualM (pure a >>= k) (k a)

        -- Test case 2: k throws an error
        prop "Left Identity (k throws error): pure a >>= (throwError . k_err) === throwError (k_err a)" $
            \(a :: Int) (Fn (k_err :: Int -> BuildError)) -> -- Generate a function returning an error
                let k :: Int -> TestM Bool -- Still results in TestM Bool (due to error)
                    k = throwError . k_err -- Construct k inside
                in assertEqualM (pure a >>= k) (k a)


        -- Right Identity: m >>= pure === m (These were likely okay, but checking)
        prop "Right Identity: m >>= pure === m (testing with pure)" $
            \(a :: Int) -> let m = pure a :: TestM Int in assertEqualM (m >>= pure) m

        prop "Right Identity: m >>= pure === m (testing with error)" $
            \(e :: BuildError) -> let m = throwError e :: TestM Int in assertEqualM (m >>= pure) m


        -- Associativity: (m >>= f) >>= g === m >>= (\x -> f x >>= g)
        -- Test case 1: m=pure, f returns pure, g returns pure
        prop "Associativity (pure >>= (pure . f') >>= (pure . g'))" $
            \(a :: Int) (Fn (f' :: Int -> Bool)) (Fn (g' :: Bool -> Char)) ->
                let m = pure a :: TestM Int
                    f = pure . f' :: Int -> TestM Bool -- Construct f
                    g = pure . g' :: Bool -> TestM Char -- Construct g
                in assertEqualM ((m >>= f) >>= g) (m >>= (\x -> f x >>= g))

        -- Test case 2: m=error, f returns pure, g returns pure
        prop "Associativity (error >>= (pure . f') >>= (pure . g'))" $
            \(e :: BuildError) (Fn (f' :: Int -> Bool)) (Fn (g' :: Bool -> Char)) ->
                let m = throwError e :: TestM Int
                    f = pure . f' :: Int -> TestM Bool
                    g = pure . g' :: Bool -> TestM Char
                in assertEqualM ((m >>= f) >>= g) (m >>= (\x -> f x >>= g)) -- Error propagates early

        -- Test case 3: m=pure, f throws error, g returns pure
        prop "Associativity (pure >>= (throwError . f_err) >>= (pure . g'))" $
            \(a :: Int) (Fn (f_err :: Int -> BuildError)) (Fn (g' :: Bool -> Char)) ->
                let m = pure a :: TestM Int
                    f = throwError . f_err :: Int -> TestM Bool -- f throws error
                    g = pure . g' :: Bool -> TestM Char
                in assertEqualM ((m >>= f) >>= g) (m >>= (\x -> f x >>= g)) -- Error from f propagates

        -- Test case 4: m=pure, f returns pure, g throws error
        prop "Associativity (pure >>= (pure . f') >>= (throwError . g_err))" $
            \(a :: Int) (Fn (f' :: Int -> Bool)) (Fn (g_err :: Bool -> BuildError)) ->
                let m = pure a :: TestM Int
                    f = pure . f' :: Int -> TestM Bool
                    g = throwError . g_err :: Bool -> TestM Char -- g throws error
                in assertEqualM ((m >>= f) >>= g) (m >>= (\x -> f x >>= g)) -- Error from g propagates

        -- *** MODIFIED TESTS END HERE ***
