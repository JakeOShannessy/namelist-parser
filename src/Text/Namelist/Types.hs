{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Text.Namelist.Types where

import Data.Map (Map(..))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
-- import qualified Data.Repa as R
-- import qualified Data.Vector as V
import Data.Array
import qualified Data.Vector as V

-- |A specification for reading a namelist file. In Fortran the data types that
-- will be used when reading namelists are specified before reading, therefore
-- in order parse accurately we must specifiy what we can read ahead of time.
-- The key of the map is the group name.
type NamelistSpec = (M.Map String GroupSpec)

-- |Hold all of the information for a group (excluding the name). The key of the
-- map is the parameter name.
type GroupSpec = (M.Map String ParameterSpec)

-- |The data type of a parameter.
-- TODO: check what information Array requires (dimenions size etc.)
data ParameterSpec
    = PSString
    | PSDouble
    | PSInt
    | PSBool
    | PSArray ParameterSpec

-- |Holds the entire Namelist file, which is basically a list of namelists.
data NamelistFile = NamelistFile
    T.Text -- ^Comments. These are those that come at the start of the file.
    [Namelist]  -- ^The namelists which make up the file
    deriving (Show)

-- |Holds the name and parameters for each namelist read in from namelist file.
data Namelist = Namelist
    T.Text
    T.Text
    (Map T.Text ParameterValue)
    | NamelistMacro
    T.Text
    (Map T.Text ParameterValue)
    deriving (Eq, Show)
    -- ^Name, comments and map of parameters. Comments are considered as those that come after the namelist.

-- |The different types of parameter value.
data ParameterValue =
        ParString T.Text
        | ParDouble Double
        | ParInt Int
        | ParBool Bool
        | ParArray NamelistArray
        deriving (Eq, Show)


class ToParameterValue a where
    toParameterValue :: a -> ParameterValue
instance ToParameterValue T.Text where
    toParameterValue str = ParString str
instance ToParameterValue String where
    toParameterValue str = ParString $ T.pack str
instance ToParameterValue Bool where
    toParameterValue bool = ParBool bool
instance ToParameterValue Int where
    toParameterValue int = ParInt int
instance ToParameterValue Double where
    toParameterValue double = ParDouble double

type NamelistArray = M.Map (Int,Int) ParameterValue
-- type NamelistArray = [((Int,Int), ParameterValue]
