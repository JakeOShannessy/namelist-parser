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
import Data.List
import Data.Map (Map(..))
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Debug.Trace

import Text.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Text as PT
-- import Text.Parsec.Text (Parser)
import Text.Read
import Text.Namelist.Types
-- import Text.Namelist.Raw.Types (Range())

readNml :: FilePath -> IO (Either ParseError NamelistFile)
readNml filepath = do
    rawText <- T.readFile filepath
    pure $ parse namelistParser filepath rawText

parseNml :: T.Text -> Either ParseError NamelistFile
parseNml input = parse namelistParser "(unknown)" input

namelistParser :: (Monad m, Stream s m Char) => ParsecT s u m NamelistFile
namelistParser = do
    headerComments <- manyTill anyChar (lookAhead (try (char '&' *> count 4 letter)))
    namelists <- many namelist
    eof
    pure (NamelistFile (T.pack headerComments) namelists)

namelist :: (Monad m, Stream s m Char) => ParsecT s u m Namelist
namelist = do
    -- TODO: allow for the omission of trailing '/'
    char '&'
    name <- count 4 letter  -- TODO: is the definition really 4 letters
    spaces
    parameters <- many parameter
    let parameterMap = foldl' (\acc (k,v)-> M.insertWith combine k v acc) M.empty parameters
    -- pure (T.pack name, parameterMap)
    comments <- option "" (do
            char '/'
            comments <- manyTill anyChar
                (lookAhead ((try (do; endOfLine; spaces; char '&'; pure ())) <|> eof))
            spaces
            pure comments)
    pure (Namelist (T.pack name) (T.pack comments) parameterMap)
    where
        combine
            (ParArray arry1)
            (ParArray arry2)
            = ParArray (arry1 A.// (A.assocs arry2))
        combine _ _ = error "Parameter entered twice"

parameter :: (Monad m, Stream s m Char) =>
    ParsecT s u m (T.Text, ParameterValue)
parameter = do
    name <- paramName
    pos <- optionMaybe paramPos
    spaces
    char '='
    spaces
    values <- sepValList paramValue
    let value = case (pos,values) of
            (Nothing,[v]) -> v
            (Just posVals,[v]) -> ParArray $ buildArray (Range (RangeValInt 1) (RangeValInt 1), Single 1) [v]
            (Nothing, vs) ->  ParArray $ buildArray (Range (RangeValInt 1) (RangeValInt (length values)), Single 1) values
            (Just posVals, vs) -> ParArray $ buildArray posVals values
    many (char ',' <|> space)
    pure $ (T.pack name, value)
    <?> "parameter"

-- |Create an array given a set of Ranges and the values.
buildArray :: (Range, Range) -> [ParameterValue] -> NamelistArray
buildArray ranges values =
    let
        indices = buildArrayIndices (length values) ranges
        (xs, ys) = unzip indices
        minX = case xs of
            [] -> error $ show ranges
            x -> minimum x
        maxX = case xs of
            [] -> error ""
            x -> maximum x
        minY = case ys of
            [] -> error ""
            x -> minimum x
        maxY = case ys of
            [] -> error ""
            x -> maximum x
    in A.array ((minX,minY),(maxX,maxY)) $ zip indices values

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

sepValList :: (Monad m, Stream s m Char) => ParsecT s u m ParameterValue
    -> ParsecT s u m [ParameterValue]
sepValList p = do
    x <- p
    -- all parameter values must be of the same constructor
    xs <- many $ try (valParse p x)
    pure (x:xs)

valParse :: (Monad m, Stream s m Char) => ParsecT s u m ParameterValue
    -> ParameterValue -> ParsecT s u m ParameterValue
valParse p x = do
    v <- spaces *> {-optional-} (char ',') *> spaces *> p
    if matchPValTypes x v then pure v else fail "types in array don't match"

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
    <|> try (ParDouble <$> floatNum) -- assume all values are floats unless told otherwise
    <|> try boolean
    <?> "parameter value"

-- A boolean is either an F or T (case insensitive) followed by any series of
-- non-whitespace characters. It may also pre prepended by a period.
boolean :: (Monad m, Stream s m Char) => ParsecT s u m ParameterValue
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

floatNum :: (Monad m, Stream s m Char) => ParsecT s u m Double
floatNum = do
    -- s <- char option ' ' (char '-' <|> char '+')
    a <- simpleFloat
    b <- option "" ((:) <$> (char 'e' <|> char 'E') <*> simpleFloat)
    pure $ read (a++b)

-- float without exponentiation
-- this currently accepts an empty string, which should not be the case
simpleFloat :: (Monad m, Stream s m Char) => ParsecT s u m String
simpleFloat = do
    s <- optionMaybe (char '-' <|> char '+')
    de1s <- optionMaybe (many1 digit)
    de2s <- optionMaybe (char '.' *> many digit)
    d <- case (de1s, de2s) of
            (Just a, Just "") -> pure a
            (Just a, Just b) -> pure (a++('.':b))
            (Just a, Nothing) -> pure a
            (Nothing, Just "") -> fail "no number"
            (Nothing, Just b) -> pure ('0':'.':b)
            _ -> fail "no number"
    return $ case s of
        Just '-' ->  '-':d
        _ -> d

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
    pprint (Namelist name comments parameters)
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
    pprint (ParArray arry)     = T.intercalate "," (map (pprint .snd) $ A.assocs arry)

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

