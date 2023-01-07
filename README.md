#### unit-conversion

Simple (linear & proportional) unit conversions.

Author: Prem Muthedath

Scope
  - converts between units that are in direct linear propportional relationship, 
    such as meters-to-feet, kilograms-to-grams, etc.
  - does not handle stuff such as unit conversion of areas, volumes, as well as 
    celsius-to-fahrenheit conversion, or conversion of units related through 
    complex formula or mathematical equations.
  - conversions at this time are limited to those between units defined in the 
    `Unit` data type.

REF:
  1. Jane Street python implementation of a similiar problem @
     https://www.youtube.com/watch?v=VfbFJISCP3g.

GHC version: 8.10.4, cabal 3.40

HOW TO RUN THE PROGRAM AND TESTS:
  1. `cd` to `unit-conversion`, the top directory containing this README file, 
     and remain there to execute any of the steps below.
  2. to load the UnitConversion library into `GHCi`, type the below command at 
     the commandline & press `ENTER`:
        - `cabal v2-repl :unit-conversion`

    you can then use the library in `GHCi` to convert between available units.  
    for example, to convert 25.0 `Meters` to `Yards`, you can enter at the 
    `GHCi` prompt the following command:
        - `convertUnitIO (25.0, Meters, Yards)`

    if no possible conversion exists between the units you have specified, you 
    will likely see `Nothing` as the result.
  3. if you wish to run the tests on the unix commandline, you can do so by 
     running the below command (NOTE: if you are in `GHCi`, first exit from 
     there):
        - `cabal v2-run -f testing :unit-conversion-test`
  4. if you wish to run the tests in `GHCi` instead, you can do so by first 
     starting `GHCi` by loading the tests and then running those tests, using 
     the below 2 commands:
        - `cabal v2-repl -f testing :unit-conversion-test`
        - `main`

