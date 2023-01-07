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
  ( Unit (..)
  , Value
  , From
  , To
  , Factor
  , factors
  , convertUnit
  -- for GHCi (interactive) usage, mainly
  , convertUnitIO
  -- exposed only for testing internal functions
#ifdef TESTING
  , factorGraph
#endif
  ) where

--------------------------------------------------------------------------------
import UnitConversion.Core
import UnitConversion.IO
--------------------------------------------------------------------------------
