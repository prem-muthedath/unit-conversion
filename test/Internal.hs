{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | internal module containing functions supporting tests as well as tests for 
-- functions & generators used in tests.
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
-- Test.Tasty: https://tinyurl.com/y9ehkkht
-- Test.Tasty.HUnit: https://tinyurl.com/4bhyfpd7
-- Test.Tasty.QuickCheck: https://tinyurl.com/2e8mr9x6
-- Test.Tasty.ExpectFailure: https://tinyurl.com/bdhby7wc
-- Test.HUnit: https://tinyurl.com/3u6v74n2
-- Test.QuickCheck: https://tinyurl.com/3at73duf
-- Data.List: https://tinyurl.com/yku7ba69
-- Data.Map: https://tinyurl.com/ytx3nuab
-- Prelude: https://tinyurl.com/mpa3vknp
-- organize tests using Tasty: https://github.com/UnkindPartition/tasty
-- practical testing in haskell: /u/ jasper: https://tinyurl.com/3ajhndmt
--------------------------------------------------------------------------------
-- | ********* internal functions, generators supporting unit tests ************
--------------------------------------------------------------------------------
-- | checks a list for duplicate elements; returns `True` if the list has any.
-- duplicates check code from /u/ dfeuer (so) @ https://tinyurl.com/bdcspyhv
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- list sorting:
    -- /u/ peargreen https://tinyurl.com/2p8p2j2e (reddit)
    -- haskell-notes--compare-tuple-list-using-mappend--peargreen.lhs
    -- list sorting, roman cheplyaka @ https://tinyurl.com/2s49u8kf
noDups :: (Ord a, Eq a)
       => (a -> a -> Ordering)  -- ordering for sorting: `compare` or equivalent
       -> (a -> a -> Bool)      -- test uniqueness, using `==` or `/=`
       -> [a]                   -- input list
       -> Bool
noDups f g xs = let sorted = sortBy f xs
                in and $ zipWith g sorted (drop 1 sorted)

-- | all units.
units :: [Unit]
units = [ toEnum 0 :: Unit .. ]

-- | test if factors is non-empty.
nonEmptyFactors' :: [(From, Factor, To)] -> Assertion
nonEmptyFactors' facs =
  assertBool ("factors `" ++ show facs ++ "` is empty") $ facs /= []

-- | test if all factors are > 0.0
allFactorsGT0' :: [(From, Factor, To)] -> Assertion
allFactorsGT0' facs =
  case find (\ (_, v, _) -> v <= 0) facs of
      Nothing -> return ()
      Just f  -> assertFailure ("factor " ++ show f ++ " has value <= 0")

-- | test if factor units have no duplicate `Unit` values.
noDupFactors' :: [(From, Factor, To)] -> Assertion
noDupFactors' facs = assertBool msg $
      noDups compare (/=) [ (f, t) | (f, _, t) <- facs ]
  where msg :: String
        msg = "factors " ++ show facs ++ " has duplicate units."

-- | generate a `good` test factor graph.  this is a test mock -- a fake --
-- graph that mimics just the basic properties of the actual factor graph.
genGoodGraph :: M1.Map From [(To, Factor)]
genGoodGraph = M1.fromList $ [ genGoodGraph' u | u <- units ]
    where genGoodGraph' :: Unit -> (From, [(To, Factor)])
          genGoodGraph' x = let l   = length units - 1
                                vs  = replicate l (1.0 :: Double)
                            in (x, (zip  (filter (/= x) units) vs))

-- | test if given graph is non-empty.
nonEmptyGraph' :: M1.Map From [(To, Factor)] -> Assertion
nonEmptyGraph' graph | M1.null graph = assertFailure "empty graph"
                     | otherwise     = return ()

-- | test if every key in the graph has a non-empty list of values.
noEmptyGraphValues' :: M1.Map From [(To, Factor)] -> Assertion
noEmptyGraphValues' graph =
  let f :: From -> [To] -> Assertion
      f k tos | tos == [] = assertFailure msg
              | otherwise = return ()
              where msg :: String
                    -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
                    -- (<>) :: Semigroup a => a -> a -> a
                    msg = foldr (<>) []
                      [ "empty value: "
                      , show tos
                      , " for graph key: "
                      , show k
                      ]
      chks = [ f k (map fst vs) | (k, vs) <- M1.toList graph ]
  in processAssertions chks

-- | test if every key in the graph is NOT contained in its own values.
noCircularGraphKeys' :: M1.Map From [(To, Factor)] -> Assertion
noCircularGraphKeys' graph =
  let f :: From -> [To] -> Assertion
      f k tos | k `elem` tos = assertFailure msg
              | otherwise = return ()
              where msg :: String
                    -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
                    -- (<>) :: Semigroup a => a -> a -> a
                    msg = foldr (<>) []
                      [ "graph key `"
                      , show k
                      , "` present in its own value."
                      ]
      chks = [ f k (map fst vs) | (k, vs) <- M1.toList graph ]
  in processAssertions chks

-- | test if every key in the graph has list of values having unique `To`.
noDupGraphValues' :: M1.Map From [(To, Factor)] -> Assertion
noDupGraphValues' graph =
    let f :: From -> [To] -> Assertion
        f k tos = assertBool msg $ noDups compare (/=) tos
          where msg :: String
                -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
                -- (<>) :: Semigroup a => a -> a -> a
                msg = foldr (<>) []
                  [ "graph key `"
                  , show k
                  , "` has duplicate `To` values: "
                  , show tos
                  ]
        chks = [ f k (map fst vs) | (k, vs) <- M1.toList graph ]
    in processAssertions chks

-- | test if every value associated with a key in the graph is itself a key.
valuesGraphKeys' :: M1.Map From [(To, Factor)] -> Assertion
valuesGraphKeys' graph =
    let f :: From -> [To] -> [Assertion]
        f k tos = [ g to ( M1.lookup to graph ) | to <- tos ]
          where g :: To -> Maybe [(To, Factor)] -> Assertion
                g to Nothing   = assertFailure $ msg to
                g _ (Just _)   = return ()
                msg :: To -> String
                -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
                -- (<>) :: Semigroup a => a -> a -> a
                msg to = foldr (<>) []
                    [ "graph key `"
                    , show k
                    , "` has a value `"
                    , show to
                    , "` that is not a graph key itself."
                    ]
        chks = concat [ f k (map fst vs) | (k, vs) <- M1.toList graph ]
    in processAssertions chks

-- | test if every key is itself a value for its corresponding values.
keysGraphValues' :: M1.Map From [(To, Factor)] -> Assertion
keysGraphValues' graph =
    let f :: From -> [To] -> [Assertion]
        f k tos = [ g to ( M1.lookup to graph ) | to <- tos ]
          where g :: To -> Maybe [(To, Factor)] -> Assertion
                g _ Nothing    = error $ "bad test data; for key " ++ show k
                g to (Just vs) = assertBool (msg to) $ k `elem` (map fst vs)
                msg :: To -> String
                -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
                -- shows :: Show a => a -> ShowS
                -- showString :: String -> ShowS
                -- type ShowS = String -> String
                -- NOTE: foldr (\ x acc -> x . acc) (showString "")
                -- :: Foldable t => t (String -> String) -> String -> String
                msg to = foldr (.) (showString "")
                    [ shows to
                    , showString " is a value of graph key `"
                    , shows k
                    , showString "`, but `"
                    , shows k
                    , showString "` is missing as a value of the key: "
                    , shows to
                    ] $ []
        chks = concat [ f k (map fst vs) | (k, vs) <- M1.toList graph ]
    in processAssertions chks

-- | test if `k` is a key that has `(t, v)` has one of its values, then the 
-- graph also has a key `t` with `(k, 1.0/v)` as one of its values.  this rule 
-- should apply to every value of every key in the graph.
graphKeyValueFactorRule' :: M1.Map From [(To, Factor)] -> Assertion
graphKeyValueFactorRule' graph =
  let chks :: [Assertion]
      chks = do
          (k, kvs) :: (From, [(To, Factor)]) <- M1.toList graph
          (to, kf) :: (To, Factor)           <- kvs
          let tvs  :: Maybe [(To, Factor)] = M1.lookup to graph
          return $ f k to kf tvs
      f :: From -> To -> Factor -> Maybe [(To, Factor)] -> Assertion
      f k t kf tvs = case f' of
          Just ass -> ass
          Nothing  -> error $ "bad test data for key: " ++ show k
          where f' :: Maybe Assertion
                f' = do
                  tvs'    :: [(To, Factor)] <- tvs
                  -- find :: Foldable t => (a -> Bool) -> t a -> Maybe a
                  (_, tf) :: (To, Factor)   <- find ((== k) . fst) tvs'
                  return $ assertBool (msg tf) $ abs (kf - (1.0/tf)) <= 0.0001
                msg :: Factor -> String
                -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
                -- shows :: Show a => a -> ShowS
                -- showString :: String -> ShowS
                -- type ShowS = String -> String
                -- NOTE: foldr (\ x acc -> x . acc) (showString "")
                -- :: Foldable t => t (String -> String) -> String -> String
                msg tf = foldr (.) (showString "")
                    [ shows k
                    , showString " is a graph key related to "
                    , shows t
                    , showString " by a factor "
                    , shows kf
                    , showString ", but "
                    , shows t
                    , showString " as a key is related to "
                    , shows k
                    , showString "\n through a 'possible' wrong factor "
                    , shows tf
                    , showString ", instead of the expected value "
                    , showString "1.0/"
                    , shows kf
                    , showString " = "
                    , shows (1.0/kf)
                    ] $ []
  in processAssertions chks

-- | process `[Assertion]` and report the first `Assertion` if any.
-- throws `assertFailure` if the supplied list is empty.
-- NOTE: `type Assertion = IO ()`
processAssertions :: [Assertion] -> Assertion
processAssertions chks =
  -- NOTE: we need some way to check if `chks` is not empty.  one way is to try 
  -- the usual `chks /= []` condition, which will normally force a pattern match 
  -- on `chks`, forcing element evaluation. but this code will not work here, 
  -- because `instance Eq [IO ()]` & `instance Eq (IO ())` both do not exist.  
  -- and, by the way, even if those instances did exist, `chks /= []` condition, 
  -- by forcing element evaluation of `[Assertion]`, will throw an exception, so 
  -- we will never be able to complete the conditional check.
  --
  -- another way is to use `not $ null chks`, which will work, because it does 
  -- not require any `Eq` instances, and since `null = foldr (\_ _ -> False) 
  -- True` does not force list element evaluation, there is no risk of exception 
  -- either, allowing us to complete the conditional check safely. so that's 
  -- what we have done here.
  --
  -- by the way, we could have used `length chks /= 0` instead of `null`, and it 
  -- would have worked as well, because `length = foldl' (\c _ -> c+1) 0` does 
  -- not force element evaluation, just like `null`.  but i went with `null` 
  -- because it fits our purpose, and it terminates even for infinite lists.
  --
  -- on 'list forcing', see roman cheplyaka @ https://tinyurl.com/ycx6ytxt
  if not $ null chks
     -- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
     then do _ <- sequence chks :: IO [()]
             return ()
     else assertFailure "test not executed because `[Assertion]` is empty"

-- | test factor units exhaustively form all keys in the graph.
factorUnitsGraphKeys' :: M1.Map From [(To, Factor)] -> Assertion
factorUnitsGraphKeys' graph =
    let ks  = sort $ M1.keys graph
        fs  = sort $ nub $ concat [ [f, t] | (f, _, t) <- factors ]
        msg = "graph keys: " ++ show ks ++ " /= " ++
              "units in conversion factors: \n " ++ show fs
    in assertBool msg $ ks == fs

--------------------------------------------------------------------------------
-- | ********* test code for internal test functions, generators follows *******
--------------------------------------------------------------------------------
-- testGroup :: TestName -> [TestTree] -> TestTree
tests :: TestTree
tests = testGroup " *** Tests of internal test functions, data generators ***"
          [ qcInternal
          , unitTestsInternal
          ]

--------------------------------------------------------------------------------
-- | ************************* QC internal tests *******************************
--------------------------------------------------------------------------------
-- testGroup :: TestName -> [TestTree] -> TestTree
-- testProperty :: Testable a => TestName -> a -> TestTree
qcInternal :: TestTree
qcInternal = testGroup "QuickCheck properties -- internal test functions, generators"
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
  arbitrary = elements units

-- | generate a random list of factors having no duplicates.
genGoodFactors :: Gen [(From, Factor, To)]
genGoodFactors = do
      us <- f1
      vs <- f2 (length us)
      return $ zipWith (\ (a, b) c -> (a, c, b)) us vs
  where f1 :: Gen [(Unit, Unit)]
        f1 = nub <$> (listOf1 $ do
          x <- arbitrary :: Gen Unit
          y <- arbitrary :: Gen Unit
          return (x, y))
        f2 :: Int -> Gen [Double]
        f2 l = vectorOf l $
          (arbitrary :: Gen Double) `suchThat` (\ v -> v > 0 && v <= 100.0)

-- | generate a tuple having the same source & destination unit for conversion.
genIdentityConv :: Gen (Value, From, To)
genIdentityConv = do
  from <- arbitrary :: Gen Unit
  val  <- frequency [ (1, return 0.0)
                    , (1, return 1.0)
                    , (2, (arbitrary :: Gen Double)
                            `suchThat`
                            (\ x -> x > 0 && x <= 100.0)
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
                            (\ x -> x > 0 && x <= 100.0)
                      )
                    ]
  to   <- (arbitrary :: Gen Unit) `suchThat` (/= from)
  return (val, from, to)

--------------------------------------------------------------------------------
-- | ******* QuickCheck properties -- internal functions & QC generators *******
--------------------------------------------------------------------------------
-- | test `genGoodFactors` generator.
-- NOTE: `\ xs` instead of `\xs`, as `\ ` is a lambda function, so space after 
-- `\` is recommended, just as a normal function call is written with space as 
-- `f x`; see https://en.wikibooks.org/wiki/Haskell/More_on_functions
prop_genGoodFactors :: Property
prop_genGoodFactors = forAll genGoodFactors $
  \ xs -> let ok   = and $ map (\ (_, f, _) -> f > 0) xs
              us   = [ (u1, u2) | (u1, _, u2) <- xs ]
              uniq = nub us == us
          in ok && uniq

-- | test `noDups` function.
prop_noDups :: Property
prop_noDups = forAll genGoodFactors $
  \ xs -> (noDups f g xs) && not (noDups f g (xs ++ xs))
  where f :: (From, Factor, To) -> (From, Factor, To) -> Ordering
        -- compare :: Ord a => a -> a -> Ordering
        -- /u/ peargreen https://tinyurl.com/2p8p2j2e (reddit)
        -- haskell-notes--compare-tuple-list-using-mappend--peargreen.lhs
        -- list sorting, roman cheplyaka @ https://tinyurl.com/2s49u8kf
        f = \ (a, _, b) (c, _, d) -> if a /= c then compare a c else compare b d
        g :: (From, Factor, To) -> (From, Factor, To) -> Bool
        g = \(a, _, b) (c, _, d) -> not (a == c && b == d)

-- | test `genIdentityConv` generator.
prop_genIdentityConv :: Property
prop_genIdentityConv = forAll genIdentityConv $
  \ (v, from, to) -> v >= 0.0 && from == to

-- | test `genNonIdentityConv` generator.
prop_genNonIdentityConv :: Property
prop_genNonIdentityConv = forAll genNonIdentityConv $
  \ (v, from, to) -> v >= 0.0 && from /= to

--------------------------------------------------------------------------------
-- | ************************ unit tests -- internal  **************************
--------------------------------------------------------------------------------
unitTestsInternal :: TestTree
unitTestsInternal = testGroup
        "Unit tests -- internal test generators, functions"
        [ generator, expFail ]
  where generator = testGroup "unit tests of test data generators"
            [ testCase "test graph generator" test_genGoodGraph ]
        -- expectFail :: TestTree -> TestTree
        -- testGroup :: TestName -> [TestTree] -> TestTree
        -- testCase :: TestName -> Assertion -> TestTree
        expFail = expectFail $
          testGroup "unit tests of expected-to-fail internal test functions"
            [ testCase "`test_nonEmptyFactors'`" test_nonEmptyFactors'
            , testCase "`test_allFactorsGT0'`" test_allFactorsGT0'
            , testCase "`test_noDupFactors'`" test_noDupFactors'
            , testCase "`test_processAssertions`" test_processAssertions
            , testCase "`test_processAssertionsNil`" test_processAssertionsNil
            , testCase "`test_nonEmptyGraph'`" test_nonEmptyGraph'
            , testCase "`test_noEmptyGraphValues'`" test_noEmptyGraphValues'
            , testCase "`test_noCircularGraphKeys'`" test_noCircularGraphKeys'
            , testCase "`test_noDupGraphValues'`" test_noDupGraphValues'
            , testCase "`test_valuesGraphKeys'`" test_valuesGraphKeys'
            , testCase "`test_keysGraphValues'`" test_keysGraphValues'
            , testCase "`test_graphKeyValueFactorRule'`" test_graphKeyValueFactorRule'
            , testCase "`test_factorUnitsGraphKeys'`" test_factorUnitsGraphKeys'
            ]

--------------------------------------------------------------------------------
-- | ************ unit tests of test functions & test data generators **********
--------------------------------------------------------------------------------
-- | test the factor graph generator.
test_genGoodGraph :: Assertion
test_genGoodGraph =
         let mp = genGoodGraph
             ks = M1.keys mp
             ch = [ f k ((M1.!) mp k) | k <- ks, ks == units ]
         in assertBool "bad test graph generator" $ (ch /= []) && (and ch)
    where f :: From -> [(To, Factor)] -> Bool
          f k vs | nub vs /= vs = False
                 | (map fst vs) /= filter (/= k) units = False
                 | any (/= 1.0) (map snd vs) = False
                 | otherwise = True

-- | test `nonEmptyFactors'`
test_nonEmptyFactors' :: Assertion
test_nonEmptyFactors' = do nonEmptyFactors' good
                           nonEmptyFactors' bad
    where good = [(Meters, 2.0, Yards), (Stone, 4.0, Pounds)]
          bad  = []

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

-- | test `processAssertions`
test_processAssertions :: Assertion
test_processAssertions = do processAssertions good
                            processAssertions bad
  where good = [ assertBool "pass" True
               , assertBool "pass" True
               , assertBool "pass" True
               ]
        bad  = [ assertBool "pass" True
               , assertBool "Assertion failure because of `False` value" False
               , assertBool "pass" True
               ]

-- | test `processAssertionsNil`
test_processAssertionsNil :: Assertion
test_processAssertionsNil = processAssertions ([] :: [Assertion])

-- | test `nonEmptyGraph'`
test_nonEmptyGraph' :: Assertion
test_nonEmptyGraph' = do nonEmptyGraph' nonEmpty
                         nonEmptyGraph' empty
  where nonEmpty = genGoodGraph
        empty    = M1.empty

-- | test `noEmptyGraphValues'`
test_noEmptyGraphValues' :: Assertion
test_noEmptyGraphValues' = do noEmptyGraphValues' genGoodGraph
                              noEmptyGraphValues' bad
  where bad = M1.fromList [ (Meters, []), (Yards, [(Meters, 1.0)]) ]

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

-- | test `graphKeyValueFactorRule'`
test_graphKeyValueFactorRule' :: Assertion
test_graphKeyValueFactorRule' = do graphKeyValueFactorRule' genGoodGraph
                                   graphKeyValueFactorRule' bad
  where bad = M1.fromList [ (Meters, [ (Yards, 5.0), (Feet, 1.0) ] )
                          , (Feet, [ (Meters, 1.0) ] )
                          , (Yards, [ (Meters, 15.0) ] )
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

