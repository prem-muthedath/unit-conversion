-- unit conversion code -- in haskell.
-- this program converts a given value from `from` unit to `to` unit.
--
-- NOTE: we only consider units in direct linear proportional relationship, such 
-- as meters-to-feet, kilograms-to-grams, etc.  because of this limitation, this 
-- code does not handle stuff such as unit conversion of areas, volumes, 
-- celsius-to-fahrenheit conversion, etc.
--
-- author: Prem Muthedath
--
-- NOTE: i first saw this problem, and its solution, in a video posted by Jane 
-- Street at https://www.youtube.com/watch?v=VfbFJISCP3g
-- Jane Street implementation is in python.
--
-- HOW TO RUN:
-- 1. load this file in GHCi.
-- 2. then call `convertUnit` function at the prompt.  for example, to convert 
--    25 meters to yards, you can type the below command (without the backticks) 
--    at the GHCi prompt and press ENTER:
--
--        `convertUnit (25.0, Meters, Yards)`
--
-- 3. if a conversion is possible, you will see the result at the GHCi prompt.  
--    on the other hand, if no conversion exists, you will see "Nothing."
-- 4. Note that the units you specify at the GHCi commandline must correspond 
--    EXACTLY to the ones defined in `Unit`; otherwise, you will get an error.
-- 5. `convertUnitIO` does the same thing as (2) but prints out the results in a 
--    user-friendly format. for example, you can enter the below command 
--    (without the backticks) at GHCi prompt:
--
--        `convertUnitIO (25.0, Meters, Yards)`
--
-- 6. finally, if instead of (2), you opt to run the tests, you can do so by 
--    entering the below command (without the backticks) at the GHCi prompt:
--
--        `runTests`
--
--    the test run will output both results from running `convertUnitIO` on all 
--    test cases as well as a comparision of actual, expected, and actual vs 
--    expected comparision for each of the test cases.  if actual and expected 
--    agree within allowable tolerance, you see "True`; otherwise, "False."
--
--------------------------------------------------------------------------------
import qualified Data.Map as M1
import Data.List (sortBy)
--------------------------------------------------------------------------------
-- | defines possible units.
data Unit = Meters
              | Feet
              | Inches
              | Centimeters
              | Millimeters
              | Yards
              | Kilograms
              | Grams
              | Pounds
              | Stone
              | Wazooo    -- this is an "unreal" unit, used just for testing.
              deriving (Eq, Ord, Enum, Show)

--------------------------------------------------------------------------------
-- some type synonyms for readability.
type Factor = Double; type From = Unit; type To = Unit

--------------------------------------------------------------------------------
-- | specifies conversion factors from `from` units to `to` units.
factors :: [(From, Factor, To)]
factors = [ (Meters, 3.28084, Feet),
            (Feet, 12.0, Inches),
            (Feet, 0.333333, Yards),
            (Inches, 25.4, Millimeters),
            (Meters, 100.0, Centimeters),
            (Stone, 14.0, Pounds),
            (Kilograms, 1000.0, Grams),
            (Kilograms, 2.20462, Pounds) ]

--------------------------------------------------------------------------------
-- | graph of factors, represented using a Map.
--    1. Each `unit` in `factors` represents a `key` in the Map, and each such 
--       key represents a node in the factor graph.
--    2. each unit (i.e., key) can be "DIRECTLY" converted to one or more units, 
--       using the associated conversion factors defined in `factors`.
--    3. all these units associated with each `key` are in turn nodes connected 
--       to the `key` node in the graph.  to complete the graph between the 
--       `key` (also known as the `from` unit) and its associated nodes (also 
--       known as the `to` units), we have to also specify the conversion 
--       factors, defined by `factors`, for each node associated with the `key`.
--    4. each associated node (the to` unit) and the conversion factor form a 
--       2-tuple, and all these 2-tuples asscoaited with a node (i.e., the key, 
--       the `from` unit) are in a list.
--    5. so each key (i.e., the `from` unit) in the Map, the factor graph, maps 
--       to a list of 2-tuples, & each 2-tuple has a node (the `to` unit) and 
--       the conversion factor. this mapping defines the factor graph.
--    6. for an intuitive understanding, this graph can also be viewed as a list 
--       of edges, where each edge is a 2-tuple consisting of the `from` key and 
--       its associated list of 2-tuples of `to` nodes and conversion factors.
--    7. for a similiar graph representation, see https://tinyurl.com/4wfuvbff
factorGraph :: M1.Map From [(To, Factor)]
-- NOTES:
-- 1. list group code from /u/ daniel wagner @ https://tinyurl.com/5dfytk5j (so)
-- 2. another soln, see /u/ nikita volkov @ https://tinyurl.com/5cybbdet (so)
-- 3. `fmap` for `Map` is same as `map` for `Map`, which applies the mapping 
--    function only to the values, and not to the keys.
-- 4. M1.fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> M1.Map k a
-- 5. (.) :: (b -> c) -> (a -> b) -> a -> c
-- 6. (<$>) :: Functor f => (a -> b) -> f a -> f b
-- 7. ($) :: (a -> b) -> a -> b
factorGraph = ($ []) <$> M1.fromListWith (.) [(k, (v:)) | (k, v) <- edges]
  where edges :: [(From, (To, Factor))]
        -- NOTE:
        --  1. in below code, we also add the edge for inverse conversion using 
        --     `1.0/f`. that is, for example, if we know meters-to-feet factor 
        --     as `f`, then `1.0/f` will be the conversion factor for 
        --     feet-to-meters conversion.
        --  2.`edges` code follows /u/ chi @ https://tinyurl.com/ynvc66f3 (so)
        --  3. to ensure we have a valid graph, `edges` uses `goodFactors`, 
        --     instead of directly using `factors`. if `factors` has no 
        --     duplicates, `goodFactors` returns `factors`; else it errors.
        edges = [y | (u1, f, u2) <- goodFactors, y <- [edge u1 f u2, edge u2 (1.0/f) u1]]
        edge :: From -> Factor -> To -> (From, (To, Factor))
        edge = \u1 f u2 -> (u1, (u2, f))

--------------------------------------------------------------------------------
-- | checks `factors`, returning it if it has 0 duplicates; else, throws error.
goodFactors :: [(From, Factor, To)]
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- duplicates check code from /u/ dfeuer (so) @ https://tinyurl.com/bdcspyhv
goodFactors = let sorted = sortBy f factors
                  noDups = and $ zipWith g sorted (drop 1 sorted)
              in if noDups then factors else error "`factors` contains duplicates"
  where f :: (From, Factor, To) -> (From, Factor, To) -> Ordering
        -- compare :: Ord a => a -> a -> Ordering
        f = \(a, _, b) (c, _, d) -> if (compare a c) == EQ then (compare b d) else compare a c
        g :: (From, Factor, To) -> (From, Factor, To) -> Bool
        g = \(a, _, b) (c, _, d) -> if (a == c && b == d) then False else True

--------------------------------------------------------------------------------
-- | converts given value from one unit to another and returns the result.
-- as opposed to factors`, which only defines 'DIRECT' conversions between 2 
-- units, `convertUnit` function traverses the factor graph to convert between 
-- any 2 units, as long as such a conversion is possible, INDIRECTLY or 
-- DIRECTLY.  if no possible conversion can be done, it returns `Nothing`. 
--
-- some type synonyms for readability:
type Value = Double; type Marked = Unit

convertUnit :: (Value, From, To) -> Maybe Double
convertUnit (val, from, to)
      -- M1.notMember :: Ord k => k -> M1.Map k a -> Bool
      | M1.notMember from factorGraph = Nothing
      | otherwise                     = convert' [] [(from, val)]
      where convert' :: [Marked] -> [(Unit, Value)] -> Maybe Double
            convert' marked [] = Nothing
            convert' marked ((u1, v1) : xs)
              -- M1.notMember :: Ord k => k -> M1.Map k a -> Bool
              | M1.notMember u1 factorGraph = error $ "bad unit " ++ show u1
              | u1 == to                    = Just v1
              | u1 `elem` marked            = convert' marked xs
              | otherwise                   = do
                  -- M1.lookup :: Ord k => k -> M1.Map k a -> Maybe a
                  ys <- M1.lookup u1 factorGraph
                  let zs = [(u2, v1 * factor) | (u2, factor) <- ys]
                  convert' (u1:marked) (xs ++ zs)

--------------------------------------------------------------------------------
-- | IO version of `convertUnit`.  prints result in user-friendly format.
convertUnitIO :: (Value, From, To) -> IO ()
-- `case` idea: /u/ kqr (so) @ https://tinyurl.com/y9ypk3hv
convertUnitIO (val, from, to) = case (convertUnit (val, from, to)) of
                                    Nothing   ->  printResult "Nothing"
                                    Just x    ->  printResult $ show x
  where printResult :: String -> IO ()
        printResult res = putStrLn $
          show val ++ " " ++ show from ++ " = " ++ res ++ " " ++ show to

--------------------------------------------------------------------------------
-- | *********************** test code follows *******************************
--------------------------------------------------------------------------------
-- | run some basic tests & print results, including results vs expected.
runTests :: IO ()
runTests = do
  let cases = [ -- test cases
                (100.0, Meters, Meters),
                (25.0, Meters, Feet),
                (34.5, Feet, Inches),
                (4676.28, Inches, Feet),
                (12.0, Feet, Meters),
                (45.56, Meters, Inches),
                (96.0, Inches, Meters),
                (90.0, Feet, Yards),
                (65.52, Yards, Inches),
                (56.0, Meters, Yards),
                (45.0, Meters, Kilograms),
                (90.0, Feet, Wazooo),
                (32.89, Wazooo, Meters),
                (10.0, Inches, Millimeters),
                (15.0, Meters, Centimeters),
                (9000.0, Millimeters, Meters),
                (12.0, Feet, Centimeters),
                (3200.0, Centimeters, Meters),
                (700.0, Stone, Kilograms),
                (789.0, Pounds, Grams),
                (567.0, Pounds, Stone),
                (15.0, Kilograms, Meters),
                (1000.0, Grams, Kilograms),
                (14.0, Pounds, Stone)
              ]
      exp = [ -- expected output
              Just 100.0,         -- M -> M
              Just 82.021,        -- F -> M
              Just 414.0,         -- F -> I
              Just 389.69,        -- I -> F
              Just 3.65753,       -- F -> M
              Just 1793.700844,   -- M -> I
              Just 2.4383999,     -- I -> M
              Just 29.99997,      -- F -> Y
              Just 2358.7223587,  -- Y -> I
              Just 61.2422854,    -- M -> Y
              Nothing,            -- M -> K
              Nothing,            -- F -> W
              Nothing,            -- W -> M
              Just 253.99999,     -- I -> Mill
              Just 1500.0,        -- M -> C
              Just 9.0,           -- Mill -> M
              Just 365.759988,    -- F -> C
              Just 32.00,         -- C -> M
              Just 4445.2105124,  -- S -> K
              Just 357884.805544, -- P -> G
              Just 40.50,         -- P -> S
              Nothing,            -- K -> M
              Just 1.0,           -- G -> K
              Just 1.0            -- P -> S
            ]
      res = map (\(x, y, z) -> convertUnit (x, y, z)) cases   -- results
      -- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
      chk = zipWith (\x y -> case (x, y) of  -- check results vs expected
                              (Just a, Just b)    -> abs (a - b) <= 0.0001
                              (Nothing, Nothing)  -> True
                              (_, _)              -> False) res exp

  -- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
  -- zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
  putStrLn "***************** RESULTS (input = output) **********************"
  mapM_ convertUnitIO cases   -- print just the results in user friendly format
  putStrLn "\n **** (actual, expected, actual vs expected) ****"
  mapM_ print $ zipWith3 (\a b c -> (a, b, c)) res exp chk  -- print all

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

