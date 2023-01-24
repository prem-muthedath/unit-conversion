{-# LANGUAGE ScopedTypeVariables #-}

-- | test code for unit conversion.
-- author: Prem Muthedath
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Test.HUnit (Assertion)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import UnitConversion
import Inputs (tcs, exps)
import Internal
--------------------------------------------------------------------------------
-- Test.Tasty: https://tinyurl.com/y9ehkkht
-- Test.Tasty.HUnit: https://tinyurl.com/4bhyfpd7
-- Test.Tasty.QuickCheck: https://tinyurl.com/2e8mr9x6
-- Test.HUnit: https://tinyurl.com/3u6v74n2
-- Test.QuickCheck: https://tinyurl.com/3at73duf
-- organize tests using Tasty: https://github.com/UnkindPartition/tasty
-- practical testing in haskell: /u/ jasper: https://tinyurl.com/3ajhndmt
--------------------------------------------------------------------------------
-- | *********************** test code follows *******************************
--------------------------------------------------------------------------------
-- defaultMain :: TestTree -> IO ()
main :: IO ()
main = defaultMain Main.tests

-- | defines the tests.
-- testGroup :: TestName -> [TestTree] -> TestTree
tests :: TestTree
tests = testGroup "Tests" [Internal.tests, library]
  where library = testGroup
            "*** Tests of `unit-conversion` library functions ***"
            [unitTests, qcProps]
--------------------------------------------------------------------------------
-- | unit tests.
-- testGroup :: TestName -> [TestTree] -> TestTree
-- testCase :: TestName -> Assertion -> TestTree
unitTests :: TestTree
unitTests = testGroup "Unit Tests -- `unit-conversion` library functions"
    [ testCase "non-empty set of defined factors" nonEmptyFactors
    , testCase "positive factors" allFactorsGT0
    , testCase "unique factors" noDupFactors
    , testCase "non-empty graph" nonEmptyGraph
    , testCase "non-empty values for every key in graph" noEmptyGraphValues
    , testCase "no circular graph keys" noCircularGraphKeys
    , testCase "unique `To` values for each graph key" noDupGraphValues
    , testCase "graph values are valid keys themselves" valuesGraphKeys
    , testCase "graph keys are valid values themselves" keysGraphValues
    , testCase "graph key-value rule for factors" graphKeyValueFactorRule
    , testCase "graph keys = units in conversion factors" factorUnitsGraphKeys
    , testCase "unit conversion" testUnitConversion
    ]

-- | test if factors is non-empty.
nonEmptyFactors :: Assertion
nonEmptyFactors = nonEmptyFactors' factors

-- | test if all factors are > 0.0
allFactorsGT0 :: Assertion
allFactorsGT0 = allFactorsGT0' factors

-- | test if factor units have no duplicate `Unit` values.
noDupFactors :: Assertion
noDupFactors = noDupFactors' factors

-- | test if given graph is non-empty.
nonEmptyGraph :: Assertion
nonEmptyGraph = nonEmptyGraph' factorGraph

-- | test if every key in the graph has a non-empty list of values.
noEmptyGraphValues :: Assertion
noEmptyGraphValues = noEmptyGraphValues' factorGraph

-- | test if every key in the graph is NOT contained in its own values.
noCircularGraphKeys :: Assertion
noCircularGraphKeys = noCircularGraphKeys' factorGraph

-- | test if every key in the graph has list of values having unique `To`.
noDupGraphValues :: Assertion
noDupGraphValues = noDupGraphValues' factorGraph

-- | test if every value associated with a key in the graph is itself a key.
valuesGraphKeys :: Assertion
valuesGraphKeys = valuesGraphKeys' factorGraph

-- | test if every key is itself a value for its corresponding values.
keysGraphValues :: Assertion
keysGraphValues = keysGraphValues' factorGraph

-- | test if `k` is a key that has `(t, v)` has one of its values, then the 
-- graph also has a key `t` with `(k, 1.0/v)` as one of its values.  this rule 
-- should apply to every value of every key in the graph.
graphKeyValueFactorRule :: Assertion
graphKeyValueFactorRule = graphKeyValueFactorRule' factorGraph

-- | test factor units exhaustively form all keys in the graph.
factorUnitsGraphKeys :: Assertion
factorUnitsGraphKeys = factorUnitsGraphKeys' factorGraph

-- | test unit conversion, comparing actual vs expected for a set of test cases.
testUnitConversion :: Assertion
testUnitConversion =
    let acts :: [Maybe Value] = map convertUnit tcs   -- actuals
        chks :: [IO ()]       = zipWith f acts exps   -- check actual vs expected
        f :: Maybe Value -> Maybe Value -> Assertion
        f x@(Just a) y@(Just b) = assertBool (msg x y) $ withinTolerance a b
        f Nothing Nothing       = return ()
        f x y                   = assertBool (msg x y) False
        withinTolerance :: Value -> Value -> Bool
        withinTolerance a b = abs (a - b) <= 0.0001
        msg :: (Show a, Show b) => a -> b -> String
        msg x y = "(actual) " ++ show x ++ " /= " ++ "(expected) " ++ show y
    in processAssertions chks

--------------------------------------------------------------------------------
-- | quickcheck tests.
-- testGroup :: TestName -> [TestTree] -> TestTree
-- testProperty :: Testable a => TestName -> a -> TestTree
qcProps :: TestTree
qcProps = testGroup "QuickCheck properties -- `unit-conversion` library"
    [ testProperty "identity conversion" prop_identityConv
    , testProperty "conversion equivalence" prop_convEquivalence
    ]

-- | test unit conversion for inputs having the same source & destination units.
prop_identityConv :: Property
prop_identityConv = forAll genIdentityConv $
  \ (v, x, y) -> convertUnit (v, x, y) == Just v

-- | test unit conversion for inputs with different source & destination units.
prop_convEquivalence :: Property
prop_convEquivalence = forAll genNonIdentityConv $
  \ (val, from, to) -> let res1 = convertUnit (val, from, to)
                           res2 = convertUnit (0.0, to, from)
                           f :: Value -> Value -> Bool
                           f x y = x /= y && y > 0 && abs (y - val) <= 0.0001
                       in case res1 of
                            Nothing  -> res2 == Nothing
                            Just 0.0 -> res2 == Just 0.0
                            Just x   -> case convertUnit (x, to, from) of
                                          Nothing   -> False
                                          Just y    -> f x y

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
