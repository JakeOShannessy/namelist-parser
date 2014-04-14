-- module Tests where

-- import Data.Monoid (mempty)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.Time
-- import FDSUtilities.Parsing.OutFile
import Data.List

import System.Locale

-- import Text.ParserCombinators.Parsec

-- import FDSUtilities.Types

-- main = parseTest (parseTimeStep utc) exampleTimeStep

exampleTimeStep = "       Time Step       1   April  7, 2014  09:07:24\n       Pressure Iterations:      1\n       Maximum Velocity Error:  0.11E-01 on Mesh   2 at (  55   0   9)\n       ----------------------------------------------\n       Mesh    1, Cycle       1\n       CPU/step:     1.872 s, Total CPU:        1.87 s\n       Time step:  0.28868 s, Total time:       0.29 s\n       Max CFL number:  0.11E-01 at ( 91, 42,  3)\n       Max divergence:  0.13E-06 at (  3, 59, 13)\n       Min divergence: -0.71E-05 at ( 20, 39, 10)\n       Radiation Loss to Boundaries:       -17.132 kW\n       Mesh    2, Cycle       1\n       CPU/step:     0.374 s, Total CPU:        0.37 s\n       Time step:  0.28868 s, Total time:       0.29 s\n       Max CFL number:  0.11E-01 at ( 29, 12,  5)\n       Max divergence: -0.65E-06 at (  1, 26, 14)\n       Min divergence: -0.69E-05 at ( 52, 25, 13)\n       Radiation Loss to Boundaries:        -4.534 kW\n \n"


main = defaultMain tests

mainWithOpts = do
  -- Test options can also be specified in the code. The TestOptions
  -- type is an instance of the Monoid type class, so the easiest way
  -- to get an empty set of options is with `mempty`.
  let empty_test_opts = mempty :: TestOptions

  -- We update the empty TestOptions with our desired values.
  let my_test_opts = empty_test_opts {
    topt_maximum_generated_tests = Just 1000
    , topt_timeout = Just $ Just (4000::Int)
  }

  -- Now we create an empty RunnerOptions in the same way, and add
  -- our TestOptions to it.
  let empty_runner_opts = mempty :: RunnerOptions
  let my_runner_opts = empty_runner_opts {
    ropt_test_options = Just my_test_opts
  }

  defaultMainWithOpts tests my_runner_opts

tests = 
    [ testGroup "Parsing"
        (hUnitTestToTests outfile)
    ]
    
outfile = TestLabel "outfile" $ TestList
    [ timestepParsingTest
    ]
    
