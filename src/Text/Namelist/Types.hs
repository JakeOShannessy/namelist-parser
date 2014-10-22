{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Text.Namelist.Types where

import Data.Map (Map(..))
import qualified Data.Map as M
-- import qualified Data.Repa as R
-- import qualified Data.Vector as V
import Data.Array

-- |Holds the entire Namelist file, which is basically a list of namelists.
data NamelistFile = NamelistFile
    String -- ^Comments. These are those that come at the start of the file.
    [Namelist]  -- ^The namelists which make up the file

-- |Holds the name and parameters for each namelist read in from namelist file.
data Namelist = Namelist
    String
    String
    (Map String ParameterValue)
    deriving (Eq)
    -- ^Name, comments and map of parameters. Comments are considered as those that come after the namelist.

-- |The different types of parameter value.
data ParameterValue =
        ParString String
        | ParDouble Double
        | ParInt Int
        | ParBool Bool
        | ParArray NamelistArray
        deriving (Eq)


class ToParameterValue a where
    toParameterValue :: a -> ParameterValue
instance ToParameterValue String where
    toParameterValue str = ParString str
instance ToParameterValue Bool where
    toParameterValue bool = ParBool bool
instance ToParameterValue Int where
    toParameterValue int = ParInt int
instance ToParameterValue Double where
    toParameterValue double = ParDouble double
        
type NamelistArray = Array (Int,Int) ParameterValue
-- type NamelistArray = [((Int,Int), ParameterValue]
