-- | core unit conversion code.
--  1. does simple (linear & proportional) unit conversion.
-- `2. converts a given value from `from` unit to `to` unit.
--
-- author: Prem Muthedath
--
-- NOTES:
--  1. we only consider units in direct linear proportional relationship, such 
--     as meters-to-feet, kilograms-to-grams, etc.  because of this limitation, 
--     this code does not handle stuff such as unit conversion of areas, 
--     volumes, celsius-to-fahrenheit conversion, etc.
--
--  2. i first saw this problem, and its solution, in a video posted by Jane 
--     Street at https://www.youtube.com/watch?v=VfbFJISCP3g. Jane Street 
--     implementation is in python.
--------------------------------------------------------------------------------
module UnitConversion.Core
  ( Unit (..)
  , Value
  , From
  , To
  , Factor
  , factors
  , factorGraph
  , convertUnit
  ) where
--------------------------------------------------------------------------------
import qualified Data.Map as M1
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
-- NOTE: `factors` should /= [] & should have no duplicates; duplicates here 
-- means that no 2 elements should have the same `from` and `to` units.
factors :: [(From, Factor, To)]
factors = [ -- length factors
            (Meters, 3.28084, Feet),
            (Feet, 12.0, Inches),
            (Feet, 0.333333, Yards),
            (Inches, 25.4, Millimeters),
            (Meters, 100.0, Centimeters),
            -- weight factors
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
        --     instead of directly using `factors`. if `factors` /= [] & has no 
        --     duplicates, `goodFactors` returns `factors`; else it errors.
        edges = [y | (u1, f, u2) <- factors, y <- [edge u1 f u2, edge u2 (1.0/f) u1]]
        edge :: From -> Factor -> To -> (From, (To, Factor))
        edge = \u1 f u2 -> (u1, (u2, f))

--------------------------------------------------------------------------------
-- | converts given value from one unit to another and returns the result.
-- as opposed to factors`, which only defines 'DIRECT' conversions between 2 
-- units, `convertUnit` function traverses the factor graph (BFS algorithm) to 
-- convert between any 2 units, as long as such a conversion is possible, 
-- INDIRECTLY or DIRECTLY.  if no such conversion exists, it returns `Nothing`. 
--
-- some type synonyms for readability:
type Value = Double; type Marked = Unit

convertUnit :: (Value, From, To) -> Maybe Value
convertUnit (val, from, to)
      -- M1.notMember :: Ord k => k -> M1.Map k a -> Bool
      | M1.notMember from factorGraph = Nothing
      | otherwise                     = convert' [] [(from, val)]
      where convert' :: [Marked] -> [(Unit, Value)] -> Maybe Value
            convert' _ [] = Nothing
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
