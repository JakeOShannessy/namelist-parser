-- {-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Array as A
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid (mempty)
import System.Environment

import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Text.Parsec
import Text.Namelist

import FDSSpec (fdsSpec)

import Data.List
import qualified Data.Text as  T

import System.FilePath
import System.FilePath.Glob

-- default (T.Text)

main = do
    putStrLn "Running tests..."
    fdsExamplePaths <- globDir1 (compile "**/*.fds") "Examples"
    -- mapM_ print fdsExamplePaths
    defaultMain (tests fdsExamplePaths)

mainWithOpts = do

    -- Test options can also be specified in the code. The TestOptions
    -- type is an instance of the Monoid type class, so the easiest way
    -- to get an empty set of options is with `mempty`.
    let empty_test_opts = mempty :: TestOptions

    -- We update the empty TestOptions with our desired values.
    let my_test_opts = empty_test_opts
            { topt_maximum_generated_tests = Just 1000
            , topt_timeout = Just $ Just (4000::Int)
            }

    -- Now we create an empty RunnerOptions in the same way, and add
    -- our TestOptions to it.
    let empty_runner_opts = mempty :: RunnerOptions
    let my_runner_opts = empty_runner_opts
            { ropt_test_options = Just my_test_opts
            }
    fdsExamplePaths <- globDir1 (compile "**/*.fds") "Examples"
    -- fdsExamplePaths <- globDir1 (compile "**.fds") "Examples"
    defaultMainWithOpts (tests fdsExamplePaths) my_runner_opts

tests fdsExamplePaths =
    [ testGroup "Read simple example files" $
        (hUnitTestToTests simpleReadFileTests)
    , testGroup "Snippet parse tests"
        $ hUnitTestToTests snippetTests
    , testGroup "FDS example files"
        $ hUnitTestToTests (readExampleFileTests fdsExamplePaths)
    , testGroup "Float Parsing" $ hUnitTestToTests floatParsing
    ]
snippetTests = TestList
    [ TestLabel "Snippet1" $ parse (namelist (M.fromList [("RAMP", M.fromList [("ID", PSString),("Z", PSDouble),("F", PSDouble)])])) "Snippet1" "&RAMP ID='prof', Z= 0 F= 0 /"
        ~?= Right (Namelist (T.pack "RAMP") (T.pack "") (M.fromList [(T.pack "ID",ParString (T.pack "prof")),(T.pack "Z",ParInt 0),(T.pack "F",ParInt 0)]))
    , let spec = M.fromList
            [ ("SURF", M.fromList
                [ ("ID", PSString)
                , ("MASS_FLUX", PSArray PSDouble)
                , ("TAU_MF", PSArray PSDouble)
                , ("SPEC_ID", PSArray PSString)
                , ("COLOR", PSString)
                ])
            ]

      in TestLabel "Snippet2" $ parse (namelist spec <* eof) "Snippet2" "&SURF ID = 'FUEL SLOT',   MASS_FLUX = 0.040, TAU_MF = 0.01, SPEC_ID = 'METHANE', COLOR = 'RED' /"
            ~?= Right (Namelist
                (T.pack "SURF")
                (T.pack "")
                (M.fromList
                    [ (T.pack "ID",ParString (T.pack "FUEL SLOT"))
                    , (T.pack "COLOR",ParString (T.pack "RED"))
                    , (T.pack "MASS_FLUX",ParArray (M.fromList [((1,1),ParDouble 4.0e-2)]))
                    , (T.pack "TAU_MF",ParArray (M.fromList [((1,1),ParDouble 0.01)]))
                    , (T.pack "SPEC_ID",ParArray (M.fromList [((1,1),ParString (T.pack "METHANE"))]))
                    ]))

    , let spec = M.fromList
            [ ("SURF", M.fromList
                [ ("ID", PSString)
                , ("MASS_FLUX", PSArray PSDouble)
                , ("TAU_MF", PSArray PSDouble)
                , ("SPEC_ID", PSArray PSString)
                , ("COLOR", PSString)
                ])
            ]

      in TestLabel "Snippet3" $ parse (namelist spec <* eof) "Snippet3" "&SURF ID = 'FUEL SLOT',   MASS_FLUX(1) = 0.040, TAU_MF(1) = 0.01, SPEC_ID(1) = 'METHANE', COLOR = 'RED' /"
            ~?= Right (Namelist
                (T.pack "SURF")
                (T.pack "")
                (M.fromList
                    [ (T.pack "ID",ParString (T.pack "FUEL SLOT"))
                    , (T.pack "COLOR",ParString (T.pack "RED"))
                    , (T.pack "MASS_FLUX",ParArray (M.fromList [((1,1),ParDouble 4.0e-2)]))
                    , (T.pack "TAU_MF",ParArray (M.fromList [((1,1),ParDouble 0.01)]))
                    , (T.pack "SPEC_ID",ParArray (M.fromList [((1,1),ParString (T.pack "METHANE"))]))
                    ]))

    -- , TestLabel "Snippet2" $ parse namelist "Snippet2" "&RAMP ID='prof', Z= 0, F= 0 /"
    --     ~?= Right (Namelist (T.pack "RAMP") (T.pack "") (M.fromList [(T.pack "ID",ParString (T.pack "prof")),(T.pack "Z",ParInt 0),(T.pack "F",ParInt 0)]))
    -- , TestLabel "Snippet3" $ parse parameter "Snippet3" "Z= 0"
    --     ~?= Right (T.pack "Z",ParInt 0)
    -- , TestLabel "Snippet4" $ parse parameter "Snippet4" "MATL_ID(1,:) = 'MAT_A'"
    --     ~?= Right (T.pack "MATL_ID",ParArray (M.fromList [((1,1),ParString (T.pack "MAT_A"))]))
    -- , TestLabel "Ignore multiple commas" $ parse namelist "Snippet5" "&RAMP ID='prof',, Z= 0, F= 0 /"
    --     ~?= Right (Namelist (T.pack "RAMP") (T.pack "") (M.fromList [(T.pack "ID",ParString (T.pack "prof")),(T.pack "Z",ParInt 0),(T.pack "F",ParInt 0)]))
    -- , TestLabel "Double and single quoted strings" $ parse namelist "Snippet6" "&PROP ID='ROSIN-RAMMLER' PART_ID='ROSIN-RAMMLER' QUANTITY=\"DIAMETER\" /"
    --     ~?= Right (Namelist (T.pack "PROP") (T.pack "") (M.fromList [(T.pack "ID",ParString (T.pack "ROSIN-RAMMLER")),(T.pack "PART_ID",ParString (T.pack "ROSIN-RAMMLER")),(T.pack "QUANTITY",ParString (T.pack "DIAMETER"))]))
    -- , TestLabel "Snippet7" $ parse namelist "Snippet7" "&SURF ID='HOT'\n      DEFAULT=.TRUE.\n      TMP_FRONT = 1000.\n      TAU_T = 0.0 /"
    --     ~?= Right (Namelist (T.pack "SURF") (T.pack "") (M.fromList [(T.pack "ID",ParString (T.pack "HOT")),(T.pack "DEFAULT",ParBool True),(T.pack "TMP_FRONT",ParDouble 1000),(T.pack "TAU_T",ParDouble 0)]))
    -- , TestLabel "Comma separated parameter value array" $ parse parameter "Snippet8" "XB=110,200,-225,-175,0,50"
    --     ~?= Right (T.pack "XB",ParArray (M.fromList
    --         [ ((1,1),ParInt 110)
    --         , ((2,1),ParInt 200)
    --         , ((3,1),ParInt (-225))
    --         , ((4,1),ParInt (-175))
    --         , ((5,1),ParInt 0)
    --         , ((6,1),ParInt 50)
    --         ]))
    -- -- we cannot allow space separated arrays and also fulfill other options
    -- , TestLabel "Space separated parameter value array" $ parse parameter "Snippet9" "XB=110,200 -225,-175,0,50"
    --     ~?= Right (T.pack "XB",ParArray (M.fromList
    --         [ ((1,1),ParInt 110)
    --         , ((2,1),ParInt 200)
    --         , ((3,1),ParInt (-225))
    --         , ((4,1),ParInt (-175))
    --         , ((5,1),ParInt 0)
    --         , ((6,1),ParInt 50)
    --         ]))
    -- , TestLabel "Test indices(1)" $ parse parameter "Snippet10" "PLOT3D_QUANTITY(1)='MASS FRACTION'"
    --     ~?= Right (T.pack "PLOT3D_QUANTITY",ParArray (M.fromList
    --         [ ((1,1),ParString (T.pack "MASS FRACTION"))
    --         ]))
    -- , TestLabel "Test indices(2)" $ parse parameter "Snippet11" "PLOT3D_QUANTITY(2)='MASS FRACTION'"
    --     ~?= Right (T.pack "PLOT3D_QUANTITY",ParArray (M.fromList
    --         [ ((2,1),ParString (T.pack "MASS FRACTION"))
    --         ]))
    -- , TestLabel "Test indices" $ parse parameter "Snippet11" "&DUMP PLOT3D_QUANTITY(1)='MASS FRACTION' PLOT3D_SPEC_ID(1)='OXYGEN' PLOT3D_QUANTITY(2)='MASS FRACTION' PLOT3D_SPEC_ID(2)='SOOT' PLOT3D_QUANTITY(3)='TEMPERATURE' /"
    --     ~?= Right (T.pack "XB",ParArray (A.array ((1,1),(6,1))
    --         [ ((1,1),ParInt 110)
    --         , ((2,1),ParInt 200)
    --         , ((3,1),ParInt (-225))
    --         , ((4,1),ParInt (-175))
    --         , ((5,1),ParInt 0)
    --         , ((6,1),ParInt 50)
    --         ]))
    ]

simpleReadFileTests = TestLabel "Simple Examples" $ TestList
    [
    -- TestLabel "Test A" $ TestCase $ do
    --     Right nmlFile <- readNml "TestA.fds"
    --     return ()
    -- ,
    TestLabel "Test B" $ TestCase $ do
        parseResult <- readNml fdsSpec "TestB.fds"
        case parseResult of
            Left e -> error $ show e
            Right nmlFile -> pure ()
    ]

-- |A list of files that should fail to parse properly.
badFiles :: [FilePath]
badFiles =
    -- Contains misspelt group and parameter names
    [ joinPath ["Examples", "Flowfields", "realizable_mass_fractions.fds"]
    -- Contains an additional ampersand after tail
    , joinPath ["Examples", "Sprinklers_and_Sprays", "sphere_drag_1.fds"]
    -- Contains an additional ampersand after tail
    , joinPath ["Examples", "Sprinklers_and_Sprays", "vegetation_drag_1.fds"]
    -- Contains an additional ampersand after tail
    , joinPath ["Examples", "Sprinklers_and_Sprays", "vegetation_drag_2.fds"]
    ]

fdsExampleTest path = TestLabel path $ TestCase $ do
        parseResult <- readNml fdsSpec path
        if path `elem` badFiles
            then case parseResult of
                    Left e -> return ()
                    Right nmlFile -> error $ "Parsing of " ++ path ++" should fail"
            else case parseResult of
                    Left e -> error $ show e
                    Right nmlFile -> return ()

readExampleFileTests fdsExamplePaths = TestLabel "FDS Examples" $ TestList
    $ map fdsExampleTest fdsExamplePaths

floatParsing = TestList
    [ TestLabel "1.0" $ case parse floatNum "input" "1.0" of
        Left e ->  error $ show e
        Right v -> v ~?= 1.0
    , TestLabel "1" $ case parse floatNum "input" "1" of
        Left e ->  error $ show e
        Right v -> v ~?= 1.0
    , TestLabel "0" $ case parse floatNum "input" "0" of
        Left e ->  error $ show e
        Right v -> v ~?= 0.0
    , TestLabel "1." $ case parse floatNum "input" "1." of
        Left e ->  error $ show e
        Right v -> v ~?= 1.0
    , TestLabel "0.5" $ case parse floatNum "input" "0.5" of
        Left e ->  error $ show e
        Right v -> v ~?= 0.5
    , TestLabel ".5" $ case parse floatNum "input" ".5" of
        Left e ->  error $ show e
        Right v -> v ~?= 0.5
    , TestLabel "1.0e1" $ case parse floatNum "input" "1.0e1" of
        Left e ->  error $ show e
        Right v -> v ~?= 1.0e1
    , TestLabel "1e1" $ case parse floatNum "input" "1e1" of
        Left e ->  error $ show e
        Right v -> v ~?= 1.0e1
    , TestLabel "0.5e1" $ case parse floatNum "input" "0.5e1" of
        Left e ->  error $ show e
        Right v -> v ~?= 0.5e1
    , TestLabel ".5e1" $ case parse floatNum "input" ".5e1" of
        Left e ->  error $ show e
        Right v -> v ~?= 0.5e1
    , TestLabel "1.e1" $ case parse floatNum "input" "1.e1" of
        Left e ->  error $ show e
        Right v -> v ~?= 1.0e1
    , TestLabel "simpleFloat empty string" $ TestCase $ case parse simpleFloat "empty string" "" of
        Left e ->  assertBool "appropriately failed" True
        Right v -> assertBool "parsed an empty string" False
    , TestLabel "simpleFloat \".\"" $ TestCase $ case parse simpleFloat "dot only" "." of
        Left e ->  assertBool "appropriately failed" True
        Right v -> assertBool "parsed an dot" False
    , TestLabel "1.e1" $ case parse floatNumS "input" "1.e1" of
        Left e ->  error $ show e
        Right v -> v ~?= "1.0e1"
    , TestLabel "1000." $ case parse floatNumS "input" "1000." of
        Left e ->  error $ show e
        Right v -> v ~?= "1000.0"
    ]