{-# LANGUAGE CPP #-}

-- | simple (linear & proportional) unit conversion.
-- author: Prem Muthedath
--------------------------------------------------------------------------------
-- REF on CPP directive:
--  1. https://guide.aelve.com/haskell/cpp-vww0qd72 (CPP overview)
--  2. https://tinyurl.com/hr2x3n8r (/u/ ralph, so) (CPP in module export)
--  3. https://tinyurl.com/2s4h9vth (cs.auckland.nz, #if, #ifdef, #endif)
--  4. https://tinyurl.com/33wmdvzd (/u/ willen van onsem, so) (CPP indentation)
module UnitConversion
  (
  -- available units specified by `Unit` data type
    Unit (..)
  -- a `Double` number that we want to convert from one unit to another or the 
  -- result of a conversion we have already performed.
  , Value
  -- the  "from" `Unit` you want to convert from.
  , From
  -- the "to" `Unit` you want to convert to
  , To
  -- the conversion factor, a `Double` number, that relates `From` & `To` units.
  , Factor
  -- all available conversion factors in this package.
  , factors
  -- converts a `Value` from `From` to `To`
  , convertUnit
  -- for GHCi (interactive) usage, mainly
  -- does the same thing as `convertUnit` but also formats and prints the result
  , convertUnitIO
  -- exposed only for testing internal functions
#ifdef TESTING
  -- graph of `factors`
  , factorGraph
#endif
  ) where

--------------------------------------------------------------------------------
import UnitConversion.Core
import UnitConversion.IO
--------------------------------------------------------------------------------
