{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | internal module containing tests for functions & generators used in tests.
-- author: Prem Muthedath
--------------------------------------------------------------------------------
module Internal where
--------------------------------------------------------------------------------
import qualified Data.Map as M1
import Data.List (sortBy, sort, nub, find)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure)
import Test.HUnit (Assertion)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import UnitConversion

--------------------------------------------------------------------------------
-- | ********* internal functions, generators supporting unit tests ************
--------------------------------------------------------------------------------
-- | checks a list for duplicate elements; returns `True` if the list has any.
-- duplicates check code from /u/ dfeuer (so) @ https://tinyurl.com/bdcspyhv
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
noDups :: (Ord a, Eq a)
       => (a -> a -> Ordering)  -- ordering for sorting: `compare` or equivalent
       -> (a -> a -> Bool)      -- test uniqueness, using `==` or `/=`
       -> [a]                   -- input list
       -> Bool
noDups f g xs = let sorted = sortBy f xs
                in and $ zipWith g sorted (drop 1 sorted)

-- | test if all factors are > 0.0
allFactorsGT0' :: [(From, Factor, To)] -> Assertion
allFactorsGT0' facs =
  case find (\(_, v, _) -> v <= 0) facs of
      Nothing -> return ()
      Just f  -> assertFailure ("factor " ++ show f ++ " has value <= 0")

-- | test if factor units have no duplicate `Unit` values.
noDupFactors' :: [(From, Factor, To)] -> Assertion
noDupFactors' facs = assertBool msg $
      noDups compare (/=) [ (f, t) | (f, _, t) <- facs ]
  where msg = "factors " ++ show facs ++ " has duplicate units."

-- | generate a `good` test factor graph.  this is a test mock -- a fake --
-- graph that mimics just the basic properties of the actual factor graph.
genGoodGraph :: M1.Map From [(To, Factor)]
genGoodGraph = M1.fromList $ [ genGoodGraph' u | u <- us ]
    where us :: [Unit]
          us = [ toEnum 0 :: Unit .. ]
          genGoodGraph' :: Unit -> (From, [(To, Factor)])
          genGoodGraph' x = let l   = length us - 1
                                vs  = replicate l (1.0 :: Double)
                            in (x, (zip  (filter (/= x) us) vs))

-- | test if given graph is empty.
emptyGraph' :: M1.Map From [(To, Factor)] -> Assertion
emptyGraph' graph | M1.null graph = assertFailure "empty graph"
                  | otherwise     = return ()

-- | test if every key in the graph has a non-empty list of values.
noEmptyGraphValues' :: M1.Map From [(To, Factor)] -> Assertion
noEmptyGraphValues' graph =
  let f :: From -> [To] -> IO ()
      f k tos | tos == [] = assertFailure msg
              | otherwise = return ()
              where msg = "empty value: " ++ show tos ++ " for graph key: " ++ show k
      chks = [ f k (map fst vs) | (k, vs) <- M1.toList graph ]
  in do _ <- sequence chks :: IO [()]
        return ()

-- | test if every key in the graph is NOT contained in its own values.
noCircularGraphKeys' :: M1.Map From [(To, Factor)] -> Assertion
noCircularGraphKeys' graph =
  let f :: From -> [To] -> IO ()
      f k tos | k `elem` tos = assertFailure msg
              | otherwise = return ()
              where msg = "graph key `" ++ show k ++ "` present in its own value."
      chks = [ f k (map fst vs) | (k, vs) <- M1.toList graph ]
  in do _ <- sequence chks :: IO [()]
        return ()

-- | test if every key in the graph has list of values having unique `To`.
noDupGraphValues' :: M1.Map From [(To, Factor)] -> Assertion
noDupGraphValues' graph =
    let f :: From -> [To] -> IO ()
        f k tos = assertBool msg $ noDups compare (/=) tos
          where msg = "graph key `" ++ show k ++ "` has duplicate `To` values: " ++ show tos
        chks = [ f k (map fst vs) | (k, vs) <- M1.toList graph ]
    in do _ <- sequence chks :: IO [()]
          return ()

-- | test if every value associated with a key in the graph is itself a key.
valuesGraphKeys' :: M1.Map From [(To, Factor)] -> Assertion
valuesGraphKeys' graph =
    let f :: From -> [To] -> [IO ()]
        f k tos = [ g to ( M1.lookup to graph ) | to <- tos ]
          where g :: To -> Maybe [(To, Factor)] -> IO ()
                g to Nothing   = assertFailure $ msg to
                g _ (Just _)   = return ()
                msg to = "graph key `" ++ show k ++ "` has a value `" ++ show to ++
                          "` that is not a graph key itself."
        chks = concat [ f k (map fst vs) | (k, vs) <- M1.toList graph ]
    in do _ <- sequence chks :: IO [()]
          return ()

-- | test if every key is itself a value for its corresponding values.
keysGraphValues' :: M1.Map From [(To, Factor)] -> Assertion
keysGraphValues' graph =
    let f :: From -> [To] -> [IO ()]
        f k tos = [ g to ( M1.lookup to graph ) | to <- tos ]
          where g :: To -> Maybe [(To, Factor)] -> IO ()
                g _ Nothing    = error $ "bad test data; failed for key " ++ show k
                g to (Just vs) = assertBool (msg to) $ k `elem` (map fst vs)
                msg to = show to ++ " is a value of graph key `" ++ show k ++
                  "` but `" ++ show k ++ "` is not a value of the key: " ++ show to
        chks = concat [ f k (map fst vs) | (k, vs) <- M1.toList graph ]
    in do _ <- sequence chks :: IO [()]
          return ()

-- | test factor units exhaustively form all keys in the graph.
factorUnitsGraphKeys' :: M1.Map From [(To, Factor)] -> Assertion
factorUnitsGraphKeys' graph =
    let ks  = sort $ M1.keys graph
        fs  = sort $ nub $ concat [ [f, t] | (f, _, t) <- factors ]
        msg = "graph keys: " ++ show ks ++ " /= " ++
              " units in conversion factors: " ++ show fs
    in assertBool msg $ ks == fs

--------------------------------------------------------------------------------
-- | ********* test code for internal test functions, generators follows *******
--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup " *** Tests of internal test functions, data generators ***"
          [ qcInternal
          , unitTestsInternal
          ]

--------------------------------------------------------------------------------
-- | ************************* QC internal tests *******************************
--------------------------------------------------------------------------------
qcInternal :: TestTree
qcInternal = testGroup "QuickCheck properties -- internal functions, generators."
    [ testProperty "generator test: `genGoodFactors`" prop_genGoodFactors
    , testProperty "generator test: `genIdentityConv`" prop_genIdentityConv
    , testProperty "generator test: `genNonIdentityConv`" prop_genNonIdentityConv
    , testProperty "`noDups` function" prop_noDups
    ]

--------------------------------------------------------------------------------
-- | *************************** QC generators *********************************
--------------------------------------------------------------------------------
-- | `Arbitrary` instance for `Unit`.
instance Arbitrary Unit where
  arbitrary = elements $ [toEnum 0 :: Unit ..]

-- | generate a random list of factors having no duplicates.
genGoodFactors :: Gen [(From, Factor, To)]
genGoodFactors = do
      us <- f1
      vs <- f2 (length us)
      return $ zipWith (\(a, b) c -> (a, c, b)) us vs
  where f1 :: Gen [(Unit, Unit)]
        f1 = nub <$> (listOf1 $ do
          x <- arbitrary :: Gen Unit
          y <- arbitrary :: Gen Unit
          return (x, y))
        f2 :: Int -> Gen [Double]
        f2 l = vectorOf l $
          (arbitrary :: Gen Double) `suchThat` (\v -> v > 0 && v <= 100.0)

-- | generate a tuple having the same source & destination unit for conversion.
genIdentityConv :: Gen (Value, From, To)
genIdentityConv = do
  from <- arbitrary :: Gen Unit
  val  <- frequency [ (1, return 0.0)
                    , (1, return 1.0)
                    , (2, (arbitrary :: Gen Double)
                            `suchThat`
                            (\x -> x > 0 && x <= 100.0)
                      )
                    ]
  return (val, from, from)

-- | generate tuple having different source & destination units for conversion.
genNonIdentityConv :: Gen (Value, From, To)
genNonIdentityConv = do
  from <- arbitrary :: Gen Unit
  val  <- frequency [ (1, return 0.0)
                    , (1, return 1.0)
                    , (2, (arbitrary :: Gen Double)
                            `suchThat`
                            (\x -> x > 0 && x <= 100.0)
                      )
                    ]
  to   <- (arbitrary :: Gen Unit) `suchThat` (/= from)
  return (val, from, to)

--------------------------------------------------------------------------------
-- | ******* QuickCheck properties -- internal functions & QC generators *******
--------------------------------------------------------------------------------
-- | test `noDups` function.
prop_noDups :: Property
prop_noDups = forAll genGoodFactors $
  \xs -> (noDups f g xs == True) && (noDups f g (xs ++ xs) == False)
  where f :: (From, Factor, To) -> (From, Factor, To) -> Ordering
        -- compare :: Ord a => a -> a -> Ordering
        -- /u/ peargreen https://tinyurl.com/2p8p2j2e (reddit)
        f = \(a, _, b) (c, _, d) -> if a /= c then compare a c else compare b d
        g :: (From, Factor, To) -> (From, Factor, To) -> Bool
        g = \(a, _, b) (c, _, d) -> not (a == c && b == d)

-- | test `genGoodFactors` generator.
prop_genGoodFactors :: Property
prop_genGoodFactors = forAll genGoodFactors $
  \xs -> let ok   = and $ map (\(_, f, _) -> f > 0) xs
             us   = [ (u1, u2) | (u1, _, u2) <- xs ]
             uniq = nub us == us
         in ok && uniq

-- | test `genIdentityConv` generator.
prop_genIdentityConv :: Property
prop_genIdentityConv = forAll genIdentityConv $
  \(v, from, to) -> v >= 0.0 && from == to

-- | test `genNonIdentityConv` generator.
prop_genNonIdentityConv :: Property
prop_genNonIdentityConv = forAll genNonIdentityConv $
  \(v, from, to) -> v >= 0.0 && from /= to

--------------------------------------------------------------------------------
-- | ************************ unit tests -- internal  **************************
--------------------------------------------------------------------------------
unitTestsInternal :: TestTree
unitTestsInternal = testGroup
                      "unit tests -- internal test generators, functions"
                      [generator, expFail]
  where generator = testGroup
          "unit test of test data generators"
          [ testCase "test graph generator" $
              assertBool "test graph generator bad" test_genGoodGraph
          ]
        expFail = expectFail $
          testGroup "units test of expected-to-fail internal test functions"
            [ testCase "`test_allFactorsGT0'`" test_allFactorsGT0'
            , testCase "`test_noDupFactors'`" test_noDupFactors'
            , testCase "`test_emptyGraph'`" test_emptyGraph'
            , testCase "`test_noEmptyGraphValues'`" test_noEmptyGraphValues'
            , testCase "`test_noCircularGraphKeys'`" test_noCircularGraphKeys'
            , testCase "`test_noDupGraphValues'`" test_noDupGraphValues'
            , testCase "`test_valuesGraphKeys'`" test_valuesGraphKeys'
            , testCase "`test_keysGraphValues'`" test_keysGraphValues'
            , testCase "`test_factorUnitsGraphKeys'`" test_factorUnitsGraphKeys'
            ]

--------------------------------------------------------------------------------
-- | ************ unit tests of test functions & test data generators **********
--------------------------------------------------------------------------------
-- | test the factor graph generator.
test_genGoodGraph :: Bool
test_genGoodGraph =
         let mp = genGoodGraph
             ks = M1.keys mp
             us = [ toEnum 0 :: Unit .. ]
             ch = [ f k ((M1.!) mp k) | k <- ks, ks == us ]
         in and ch
    where f :: From -> [(To, Factor)] -> Bool
          f k vs | nub vs /= vs = False
                 | (map fst vs) /= filter (/= k) [ toEnum 0 :: Unit .. ] = False
                 | any (/= 1.0) (map snd vs) = False
                 | otherwise = True

-- | test `allFactorsGT0'`
test_allFactorsGT0' :: Assertion
test_allFactorsGT0' = do allFactorsGT0' good  -- expected to pass
                         allFactorsGT0' bad   -- expected to fail
  where good = [(Meters, 1.0, Meters)]
        bad  = [(Meters, (-1.0), Meters)]

-- | test `noDupFactors'`
test_noDupFactors' :: Assertion
test_noDupFactors' = do noDupFactors' good    -- expected to pass
                        noDupFactors' bad     -- expected to fail
  where good = [(Meters, 1.0, Meters), (Meters, 2.0, Yards)]
        bad  = [(Meters, 1.0, Yards), (Meters, 2.0, Yards)]  -- duplicates

-- | test `emptyGraph'`
test_emptyGraph' :: Assertion
test_emptyGraph' = do emptyGraph' nonEmpty
                      emptyGraph' empty
  where nonEmpty = genGoodGraph
        empty     = M1.empty

-- | test `noEmptyGraphValues'`
test_noEmptyGraphValues' :: Assertion
test_noEmptyGraphValues' = do noEmptyGraphValues' genGoodGraph
                              noEmptyGraphValues' bad
  where bad  = M1.fromList [ (Meters, []), (Yards, [(Meters, 1.0)]) ]

-- | test `noCircularGraphKeys'`
test_noCircularGraphKeys' :: Assertion
test_noCircularGraphKeys' = do noCircularGraphKeys' genGoodGraph
                               noCircularGraphKeys' bad
  where bad = M1.fromList [ (Meters, [ (Yards, 1.0), (Feet, 1.0) ])
                          , (Stone, [ (Pounds, 2.0), (Stone, 4.0) ] )
                          ]

-- | test `noDupGraphValues'`
test_noDupGraphValues' :: Assertion
test_noDupGraphValues' = do noDupGraphValues' genGoodGraph
                            noDupGraphValues' bad
  where bad = M1.fromList
                [ (Meters, [ (Yards, 5.0), (Feet, 4.0) ])
                , (Stone, [ (Grams, 3.0), (Kilograms, 5.0), (Grams, 4.0) ] )
                ]

-- | test `valuesGraphKeys'`
test_valuesGraphKeys' :: Assertion
test_valuesGraphKeys' = do valuesGraphKeys' genGoodGraph
                           valuesGraphKeys' bad
  where bad = M1.fromList [ (Meters, [ (Yards, 5.0), (Feet, 4.0) ] )
                          , (Stone, [ (Pounds, 4.0), (Grams, 3.0) ] )
                          , (Yards, [ (Inches, 3.0), (Centimeters, 2.0) ] )
                          ]

-- | test `keysGraphValues'`
test_keysGraphValues' :: Assertion
test_keysGraphValues' = do keysGraphValues' genGoodGraph
                           keysGraphValues' bad
  where bad = M1.fromList [ (Meters, [ (Yards, 5.0), (Feet, 4.0) ] )
                          , (Stone, [ (Pounds, 4.0), (Grams, 3.0) ] )
                          , (Yards, [ (Inches, 3.0), (Centimeters, 2.0) ] )
                          ]

-- | test `factorUnitsGraphKeys'`
test_factorUnitsGraphKeys' :: Assertion
test_factorUnitsGraphKeys' = do factorUnitsGraphKeys' genGoodGraph
                                factorUnitsGraphKeys' bad
  where bad = M1.fromList [ (Meters, [ (Yards, 5.0), (Feet, 4.0) ] )
                          , (Stone, [ (Pounds, 4.0), (Grams, 3.0) ] )
                          , (Yards, [ (Inches, 3.0), (Centimeters, 2.0) ] )
                          ]
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
