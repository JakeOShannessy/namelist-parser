module Main where

import Data.Time
import FDSSpec
import Text.Namelist


-- Our benchmark harness.
main =
    readTestA

readTestA = do
    t1 <- getCurrentTime
    Right nmlFile <- readNml fdsSpec "test/TestA.fds"
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1
    return ()