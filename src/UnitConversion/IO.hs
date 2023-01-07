-- | unit conversion with easy-to-read console output.
-- usage: this is a convenience feature intended primarily for GHCi.
-- author: Prem Muthedath
--------------------------------------------------------------------------------
module UnitConversion.IO where
--------------------------------------------------------------------------------
import UnitConversion.Core (Value, From, To, convertUnit)
--------------------------------------------------------------------------------
-- | IO version of `convertUnit`.  prints result in user-friendly format.
convertUnitIO :: (Value, From, To) -> IO ()
-- `case` idea: /u/ kqr (so) @ https://tinyurl.com/y9ypk3hv
convertUnitIO (val, from, to) = case (convertUnit (val, from, to)) of
                                    Nothing   ->  printResult "Nothing"
                                    Just x    ->  printResult $ show x
  where printResult :: String -> IO ()
        printResult res = putStrLn $
          show val ++ " " ++ show from ++ " => " ++ res ++ " " ++ show to

--------------------------------------------------------------------------------
