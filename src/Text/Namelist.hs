{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ApplicativeDo     #-}
module Text.Namelist
    ( module Text.Namelist
    , module Text.Namelist.Types
    )
where

import qualified Data.Array as A
import Data.Char (toUpper)
import Data.List
import Data.Map (Map(..))
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Exception (handle, SomeException)

import Debug.Trace

import Text.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Text as PT
-- import Text.Parsec.Text (Parser)
import Text.Read
import Text.Namelist.Types
-- import Text.Namelist.Raw.Types (Range())

readNml :: NamelistSpec -> FilePath -> IO (Either ParseError NamelistFile)
readNml spec filepath = do
    rawText <- T.readFile filepath
    pure $ parse (namelistParser spec) filepath rawText

parseNml :: NamelistSpec -> T.Text -> Either ParseError NamelistFile
parseNml spec input = parse (namelistParser spec) "(unknown)" input

namelistParser :: (Monad m, Stream s m Char) => NamelistSpec -> ParsecT s u m NamelistFile
namelistParser spec = do
    headerComments <- manyTill anyChar (lookAhead (try (char '&' *> count 4 letter)))
    namelists <- many (namelist spec)
    eof
    pure (NamelistFile (T.pack headerComments) namelists)

namelist :: (Monad m, Stream s m Char) => NamelistSpec -> ParsecT s u m Namelist
namelist spec = do
    -- TODO: allow for the omission of trailing '/'
    location <- getPosition
    char '&'
    name <- many1 letter <?> "group name"
    -- Once we have the name we use the data type specified in the
    -- spec, or throw an error if it isn't recognised.
    groupSpec <- case M.lookup (map toUpper name) spec of
            Just x -> pure x
            Nothing -> fail $ "Group: " <> name <> " is not in the spec"
    spaces
    parameters <- many (parameter groupSpec)
    let parameterMap =
            foldl' (\acc (k,v)-> M.insertWith combine k v acc) M.empty parameters
    comments <- option "" (do
            char '/'
            comments <- manyTill anyChar
                (lookAhead ((try (do; endOfLine; spaces; char '&'; pure ())) <|> eof))
            spaces
            pure comments)
    pure (Namelist (T.pack name) (T.pack comments) parameterMap location)
    where
        combine
            (ParArray arry1)
            (ParArray arry2)
            = ParArray (M.union arry2 arry1)
        combine _ _ = error "Parameter entered twice"

parameter :: (Monad m, Stream s m Char) => GroupSpec ->
    ParsecT s u m (T.Text, ParameterValue)
parameter groupSpec = do
    name <- paramName
    parameterSpec <- case M.lookup (map toUpper name) groupSpec of
            Just x -> pure x
            Nothing ->  fail $ "Parameter: " <> name <> " is not in the spec"
    pos <- optionMaybe paramPos
    spaces
    char '='
    spaces
    -- Choose the parser depending on what the spec says
    value <- case parameterSpec of
        PSString -> quotedString
        PSDouble -> number
        PSInt -> number
        PSBool -> boolean
        PSArray arrayType -> do
            -- TODO: this doesn't handle single elements very well.
            let parser = case arrayType of
                    PSString -> quotedString
                    PSDouble -> number
                    PSInt -> number
                    PSBool -> boolean
                    PSArray _ -> error "Can't have nested array types"
            values <- sepValList parser
            let value = case (pos,values) of
                    -- (Nothing,[v]) -> v
                    -- (Just posVals,[v]) -> ParArray $ buildArray (Range (RangeValInt 1) (RangeValInt 1), Single 1) [v]
                    (Nothing, vs) ->  ParArray $ buildArray (Range (RangeValInt 1) (RangeValInt (length values)), Single 1) values
                    (Just posVals, vs) -> ParArray $ buildArray posVals values
            pure value
    many (char ',' <|> space)
    pure $ (T.pack name, value)
    <?> "parameter"

-- |Create an array given a set of Ranges and the values.
buildArray :: (Range, Range) -> [ParameterValue] -> NamelistArray
buildArray ranges values =
    let
        indices = buildArrayIndices (length values) ranges
    in M.fromList $ zip indices values

-- |Turn a set of Ranges into a list of indices.
buildArrayIndices :: Int -> (Range, Range) -> [(Int, Int)]
buildArrayIndices n ((Single x),(Single y)) = [(x,y)]
buildArrayIndices n ((Single x),(Range (RangeValInt y1) (RangeValInt y2))) = zip (repeat x) [y1..y2]
buildArrayIndices n ((Range (RangeValInt x1) (RangeValInt x2)), (Single y)) = zip [x1..x2] (repeat y)
buildArrayIndices n ((Range RangeValDef RangeValDef), (Single y)) = zip [1..n] (repeat y)
buildArrayIndices n ((Range (RangeValInt x1) RangeValDef), (Single y)) = zip [x1..n] (repeat y)
buildArrayIndices n ((Range RangeValDef (RangeValInt x2)), (Single y)) = zip [1..x2] (repeat y)
buildArrayIndices n ((Single y),(Range RangeValDef RangeValDef)) = zip [1..n] (repeat y)
buildArrayIndices n ((Single y),(Range (RangeValInt x1) RangeValDef)) = zip [x1..n] (repeat y)
buildArrayIndices n ((Single y),(Range RangeValDef (RangeValInt x2))) = zip [1..x2] (repeat y)
buildArrayIndices n ((Range _ _), (Range _ _)) = error "two dimensional ranges not supported"

-- |Parse a list of values using the parser p separated by spaces commas or
-- both. If a value is followed by an equal sign (after zero or more spaces)
-- then it is a parameter name and not part of the array.
sepValList :: (Monad m, Stream s m Char) => ParsecT s u m ParameterValue
    -> ParsecT s u m [ParameterValue]
-- TODO: still need to make sure that the next value is actually a value and not
-- a parameter name followed by an equals sign
sepValList p = do
    x <- p
    -- all parameter values must be of the same constructor
    xs <- many $ try (valParse p x <* notFollowedBy (spaces *> char '='))
    pure (x:xs)

valParse :: (Monad m, Stream s m Char) => ParsecT s u m ParameterValue
    -> ParameterValue -> ParsecT s u m ParameterValue
valParse p x = do
    v <- spaces *> optional (char ',') *> spaces *> p
    if matchPValTypes x v
        then pure v
        else
            -- fail "types in array don't match"
            pure v

matchPValTypes :: ParameterValue -> ParameterValue -> Bool
matchPValTypes (ParString _) (ParString _) = True
matchPValTypes (ParString _) _ = False

matchPValTypes (ParDouble _) (ParDouble _) = True
matchPValTypes (ParDouble _) _ = False

matchPValTypes (ParInt _) (ParInt _) = True
matchPValTypes (ParInt _) _ = False

matchPValTypes (ParBool _) (ParBool _) =  True
matchPValTypes (ParBool _) _ = False

matchPValTypes (ParArray _) (ParArray _) = True
matchPValTypes (ParArray _) _ = False

paramName :: (Monad m, Stream s m Char) => ParsecT s u m String
paramName = do
    initialChar <- noneOf "&=/( \n\t0123456789.,"
    remainingChars <- many (noneOf "=/( \n\t.,")
    pure $ initialChar:remainingChars
    <?> "parameter name"
-- paramName = many1 (alphaNum)

paramPos :: (Monad m, Stream s m Char) => ParsecT s u m (Range, Range)
paramPos = between (char '(') (char ')') $ do
    r1 <- parseRange
    r2 <- optionMaybe (spaces *> char ',' <* spaces *> parseRange)
    pure $ case r2 of
        Nothing -> (r1, Single 1)
        Just x -> (r1, x)

-- |Parses values in the form of "x" or "x:y" where x and y are integers, e.g. "1:2"
parseRange :: (Monad m, Stream s m Char) => ParsecT s u m Range
parseRange = Text.Parsec.choice
    [ char ':' *> pure (Range RangeValDef RangeValDef)
    , do
        val1 <- intNum
        val2 <- optionMaybe $ do
            char ':'
            intNum
        pure $ case val2 of
            Nothing -> Single val1
            Just x -> Range (RangeValInt val1) (RangeValInt x)
    ]

data Range = Single Int | Range RangeVal RangeVal deriving (Eq, Show)

data RangeVal = RangeValInt Int | RangeValDef deriving (Eq, Show)

paramValue :: (Monad m, Stream s m Char) => ParsecT s u m ParameterValue
paramValue =
    quotedString
    <|> try number
    <|> try boolean
    <?> "parameter value"


number :: (Monad m, Stream s m Char) => ParsecT s u m ParameterValue
number = do
    numS <- floatNumS
    let n = case readMaybe numS of
                Just x -> Just $ ParInt x
                Nothing -> case readMaybe numS of
                    Just x -> Just $ ParDouble x
                    Nothing -> Nothing
    case n of
        Just x -> pure x
        Nothing -> fail "not a number"

-- A boolean is either an F or T (case insensitive) followed by any series of
-- non-whitespace characters. It may also pre prepended by a period.
boolean :: (Monad m, Stream s m Char) => ParsecT s u m ParameterValue
-- boolean = char '.'*>
--     ((string "FALSE." *> pure (ParBool False))
--     <|> (string "TRUE." *> pure (ParBool True)))
boolean = do
    optional (char '.')
    cs <- Text.Parsec.choice (fmap char ['t','T','f','F'])
    many (noneOf " \t\r\n/,")
    pure $ case cs of
        't' -> ParBool True
        'T' -> ParBool True
        'f' -> ParBool False
        'F' -> ParBool False

-- floatNum :: (Monad m, Stream s m Char) => ParsecT s u m Double
-- floatNum = signage <*> (read <$> many1 (oneOf "0123456789-+Ee."))

floatNumS :: (Monad m, Stream s m Char) => ParsecT s u m String
floatNumS = do
    a <- simpleFloat
    b <- option "" ((:) <$> (char 'e' <|> char 'E') <*> intNumS)
    pure $ a++b

-- float without exponentiation
-- this currently accepts an empty string, which should not be the case
simpleFloat :: (Monad m, Stream s m Char) => ParsecT s u m String
simpleFloat = do
    s <- optionMaybe (char '-' <|> char '+')
    de1s <- optionMaybe (many1 digit)
    de2s <- optionMaybe (char '.' *> many digit)
    d <- case (de1s, de2s) of
            (Just a, Just "") -> pure (a++(".0"))
            (Just a, Just b) -> pure (a++('.':b))
            (Just a, Nothing) -> pure a
            (Nothing, Just "") -> fail "no number"
            (Nothing, Just b) -> pure ('0':'.':b)
            _ -> fail "no number"
    return $ case s of
        Just '-' ->  '-':d
        _ -> d

intNumS :: (Monad m, Stream s m Char) => ParsecT s u m String
intNumS = do
    s <- optionMaybe (char '-' <|> char '+')
    ds <- many1 digit
    return $ case s of
        Just '-' ->  '-':ds
        _ -> ds

floatNum :: (Monad m, Stream s m Char) => ParsecT s u m Double
floatNum = read <$> floatNumS

quotedString :: (Monad m, Stream s m Char) => ParsecT s u m ParameterValue
quotedString = quotedStringS '\'' <|> quotedStringS '\"'

quotedStringS :: (Monad m, Stream s m Char) => Char -> ParsecT s u m ParameterValue
quotedStringS c = (ParString . T.pack) <$> between (char c) (char c) (many (noneOf [c]))

quotedContent :: (Monad m, Stream s m Char) => Char -> ParsecT s u m String
quotedContent c = many (quotedChar c)

quotedChar :: (Monad m, Stream s m Char) => Char -> ParsecT s u m Char
quotedChar c  = noneOf [c]

intNum :: (Monad m, Stream s m Char) => ParsecT s u m Int
intNum = signage <*> nat <?> "Int"

signage :: (Monad m, Stream s m Char, Num a) => ParsecT s u m (a -> a)
signage = (const negate <$> char '-') <|> (const id <$> char '+') <|> pure id

nat :: (Monad m, Stream s m Char) => ParsecT s u m Int
nat = read <$> many1 digit

readIntMaybe :: String -> Maybe Int
readIntMaybe = readMaybe

readFloatMaybe :: String -> Maybe Double
readFloatMaybe = readMaybe

-- OUT
-- |Render a Namelist
instance PPrint NamelistFile where
    pprint (NamelistFile comments nmls) = T.intercalate "" (map pprint nmls)

-- |Render a Namelist
instance PPrint Namelist where
    -- show (Namelist name comments []) = "&" ++ name ++ " " ++ "/" ++ comments ++ "\n"
    pprint (Namelist name comments parameters location)
        = "&" <> name <> " "
        <> (T.intercalate ",\n      " (renderParameters parameterList))
        <> " /" <> comments <> "\n"
        where
            parameterList = M.toList parameters

-- |Render a list of Parameters
renderParameters :: [(T.Text, ParameterValue)] -> [T.Text]
renderParameters parameters = map renderParameter parameters
renderParameter :: (T.Text, ParameterValue) -> T.Text
renderParameter (name, value) = name <> "=" <>  (pprint value)

-- |Render a Parameter
-- instance Show Parameter where
    -- show (Parameter name value) = name ++ "=" ++  (show value)
    -- --use intercalate to add commas to lists

class PPrint a where
    pprint :: a -> T.Text

-- |Convert the Haskell types into appropriate namelist strings using Show class
instance PPrint ParameterValue where
    pprint (ParString s)      = "\'" <> s <> "\'"
    pprint (ParDouble n)      = pprint n
    pprint (ParInt n)         = pprint n
    pprint (ParBool True)     = ".TRUE."
    pprint (ParBool False)    = ".FALSE."
    -- TODO: fix this printing
    pprint (ParArray arry)     = T.intercalate "," (map (pprint . snd) $ M.assocs arry)

-- instance Show ParameterValue where
--     show (ParString s)      = "\'" <> show s <> "\'"
--     show (ParDouble n)      = show n
--     show (ParInt n)         = show n
--     show (ParBool True)     = ".TRUE."
--     show (ParBool False)    = ".FALSE."
--     -- TODO: fix this printing
--     show (ParArray arry)     = intercalate "," (map (show .snd) $ A.assocs arry)


instance PPrint a => PPrint [a] where
    pprint ls = "[" <> T.intercalate "," (map pprint ls) <> "]"

instance PPrint Double where
    pprint d = T.pack $ show d

instance PPrint Int where
    pprint d = T.pack $ show d

-- |Create the text in namelist format
createNmlText :: NamelistFile -> T.Text
createNmlText (NamelistFile comments nmlLists) = T.concat (map pprint nmlLists)

-- |Write namelist data to a namelist file
writeNml :: FilePath -> NamelistFile -> IO ()
writeNml filename nmlFile = do
    T.writeFile filename (createNmlText nmlFile)

