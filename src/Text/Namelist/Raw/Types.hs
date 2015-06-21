{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Text.Namelist.Types where

import Data.Map (Map(..))
import qualified Data.Map as M
import qualified Data.Text as T
-- import qualified Data.Repa as R
-- import qualified Data.Vector as V
import Data.Array
import qualified Data.Vector as V

-- |Holds the entire Namelist file, which is basically a list of namelists.
data RawNamelistFile = RawNamelistFile
    T.Text -- ^Comments. These are those that come at the start of the file.
    [RawNamelist]  -- ^The namelists which make up the file
    deriving (Show, Eq)

-- |Holds the name and parameters for each namelist read in from namelist file.
data RawNamelist = RawNamelist
    T.Text
    T.Text
    [RawParameter]
    deriving (Show, Eq)
    -- ^Name, comments and map of parameters. Comments are considered as those that come after the namelist.

data RawParameter = RawParameter T.Text (Maybe Position) [ParameterValue] deriving (Show, Eq)

data Position = Position1 Range | Position2 Range Range deriving (Show, Eq)

data Range = Single Int | Interval Int Int deriving (Show, Eq)
    
-- |The different types of parameter value.
data ParameterValue =
        ParString T.Text
        | ParDouble Double
        | ParInt Int
        | ParBool Bool
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