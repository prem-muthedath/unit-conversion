-- | test code for unit conversion.
-- author: Prem Muthedath
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import qualified Data.Map as M1
import Data.List (sortBy)

import UnitConversion
--------------------------------------------------------------------------------
-- | *********************** test code follows *******************************
--------------------------------------------------------------------------------
main :: IO ()
main = do checkData
          runTests
  where checkData :: IO ()
        checkData = do
          putStrLn "***************** DATA CHECKS **********************"
          let x = goodFactors
              y = factorGraph
          if x /= [] then putStrLn "good factors" else putStrLn "bad factors"
          if M1.toList y /= [] then putStrLn "good graph" else putStrLn "empty graph"

-- | run some basic tests & print results, including results vs expected.
runTests :: IO ()
runTests =
  let tcs =  [ -- test cases
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
      exps = [ -- expected output
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
  in do printResults tcs
        printActualVsExpected tcs exps
  where -- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
        -- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
        -- zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
        printResults :: [(Value, From, To)] -> IO ()
        printResults tcs' = do
          putStrLn "***************** RESULTS (input => output) **********************"
          mapM_ convertUnitIO tcs'   -- print just the results in user friendly format
        printActualVsExpected :: [(Value, From, To)] -> [(Maybe Value)] -> IO ()
        printActualVsExpected tcs' exps' =
          let acts = map (\(x, y, z) -> convertUnit (x, y, z)) tcs' -- actuals
              tol  = 0.0001 :: Double   -- tolerance
              chks = zipWith (\x y -> case (x, y) of  -- check actual vs expected
                        (Just a, Just b)    -> if abs (a - b) <= tol
                                                  then "PASS"
                                                  else "FAIL"
                        (Nothing, Nothing)  -> "PASS"
                        (_, _)              -> "FAIL") acts exps'
          in do putStrLn "\n **** (actual, expected, pass/fail) ****"
                mapM_ print $ zipWith3 (\a b c -> (a, b, c)) acts exps' chks
--------------------------------------------------------------------------------
-- | checks `factors`, returning it if /= [] & has 0 duplicates; else, errors.
goodFactors :: [(From, Factor, To)]
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- duplicates check code from /u/ dfeuer (so) @ https://tinyurl.com/bdcspyhv
goodFactors | factors == [] = error "`factors` has no factors."
            | otherwise     = let sorted = sortBy f factors
                                  noDups = and $ zipWith g sorted (drop 1 sorted)
                              in if noDups
                                    then factors
                                    else error "`factors` contains duplicates"
  where f :: (From, Factor, To) -> (From, Factor, To) -> Ordering
        -- compare :: Ord a => a -> a -> Ordering
        f = \(a, _, b) (c, _, d) -> if (compare a c) == EQ then (compare b d) else compare a c
        g :: (From, Factor, To) -> (From, Factor, To) -> Bool
        g = \(a, _, b) (c, _, d) -> if (a == c && b == d) then False else True

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

