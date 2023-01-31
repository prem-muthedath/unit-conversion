-- | unit test cases and expected output.
-- author: Prem Muthedath
--------------------------------------------------------------------------------
module Inputs where
--------------------------------------------------------------------------------
import UnitConversion (Value, From, To, Unit (..))
--------------------------------------------------------------------------------
-- | test cases.
tcs :: [(Value, From, To)]
tcs = [ (100.0, Meters, Meters)
      , (25.0, Meters, Feet)
      , (34.5, Feet, Inches)
      , (4676.28, Inches, Feet)
      , (12.0, Feet, Meters)
      , (45.56, Meters, Inches)
      , (96.0, Inches, Meters)
      , (90.0, Feet, Yards)
      , (65.52, Yards, Inches)
      , (56.0, Meters, Yards)
      , (45.0, Meters, Kilograms)
      -- , (90.0, Feet, Wazooo)
      -- , (32.89, Wazooo, Meters)
      , (10.0, Inches, Millimeters)
      , (15.0, Meters, Centimeters)
      , (9000.0, Millimeters, Meters)
      , (12.0, Feet, Centimeters)
      , (3200.0, Centimeters, Meters)
      , (700.0, Stone, Kilograms)
      , (789.0, Pounds, Grams)
      , (567.0, Pounds, Stone)
      , (15.0, Kilograms, Meters)
      , (1000.0, Grams, Kilograms)
      , (14.0, Pounds, Stone)
      , (-15.8, Yards, Inches)
      , (0.0, Pounds, Stone)
      ]

--------------------------------------------------------------------------------
-- | expected output for test cases.
exps :: [Maybe Value]
exps = [ Just 100.0         -- M -> M
       , Just 82.021        -- F -> M
       , Just 414.0         -- F -> I
       , Just 389.69        -- I -> F
       , Just 3.65753       -- F -> M
       , Just 1793.700844   -- M -> I
       , Just 2.4383999     -- I -> M
       , Just 29.99997      -- F -> Y
       , Just 2358.7223587  -- Y -> I
       , Just 61.2422854    -- M -> Y
       , Nothing            -- M -> K
       -- , Nothing         -- F -> W
       -- , Nothing         -- W -> M
       , Just 253.99999     -- I -> Mill
       , Just 1500.0        -- M -> C
       , Just 9.0           -- Mill -> M
       , Just 365.759988    -- F -> C
       , Just 32.00         -- C -> M
       , Just 4445.2105124  -- S -> K
       , Just 357884.805544 -- P -> G
       , Just 40.50         -- P -> S
       , Nothing            -- K -> M
       , Just 1.0           -- G -> K
       , Just 1.0           -- P -> S
       , Nothing            -- Y -> I
       , Just 0.0           -- P -> S
       ]

--------------------------------------------------------------------------------
