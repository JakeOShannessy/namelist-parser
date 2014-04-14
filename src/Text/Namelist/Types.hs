module Text.Namelist.Types where

import Data.Map (Map(..))
import qualified Data.Map as M
-- import qualified Data.Repa as R
import qualified Data.Vector as V

-- |Holds the entire Namelist file, which is basically a list of namelists
data NamelistFile = NamelistFile
    [Namelist]  -- ^The namelists which make up the file

-- |Holds the name and parameters for each namelist read in from namelist file
data Namelist = Namelist
    String
    String
    (Map String ParameterValue)-- ^Name, comments and List of Parameters

-- -- |Holds the name and value of a parameter
-- data Parameter = Parameter
    -- String     
    -- ParameterValue  -- ^Name and Value

-- |The different types of parameter value
data ParameterValue =
        ParString String
        | ParDouble Double
        | ParInt Int
        | ParBool Bool
        | ParameterArray ((Int,Int),(Int,Int)) (V.Vector ParameterValue)
        --comment | ArrayEmpty