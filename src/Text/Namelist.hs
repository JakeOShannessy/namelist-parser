{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
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

import Text.Parsec
import Text.Parsec.Text as PT
import Text.Read
import Text.Namelist.Types


readNml :: FilePath -> IO (Either ParseError NamelistFile)
readNml filepath = do
    rawText <- T.readFile filepath
    return $ parse namelistParser filepath rawText

readNmlOfType :: T.Text -> FilePath -> IO (Either ParseError NamelistFile)
readNmlOfType nmlName filepath = do
    rawText <- T.readFile filepath
    return $ parse (namelistParserOfType nmlName) filepath rawText

parseNml :: T.Text -> Either ParseError NamelistFile
parseNml input = parse namelistParser "(unknown)" input

parseNmlOfType :: T.Text -> T.Text -> Either ParseError NamelistFile
parseNmlOfType nmlName input = parse (namelistParserOfType nmlName) "(unknown)" input

namelistParserOfType :: T.Text -> Parser NamelistFile
namelistParserOfType nmlName = do
    headerComments <- many (noneOf "&")
    namelists <- many (namelistOfType nmlName)
    eof
    return (NamelistFile (T.pack headerComments) namelists)

namelistParser :: Parser NamelistFile
namelistParser = do
    headerComments <- many (noneOf "&")
    namelists <- many namelist
    eof
    return (NamelistFile (T.pack headerComments) namelists)

eol :: Parser ()
eol = do (try (string "\r\n")) <|> (string "\n")
         return ()
    <?> "end of line"

namelist :: Parser Namelist
namelist = do
        (name, parameters) <- between (char '&') (char '/') namelistContent
        comments <- manyTill anyChar (lookAhead ((try (do; eol; spaces; char '&'; return ())) <|> eof))
        spaces
        return (Namelist name (T.pack comments) parameters)

namelistOfType :: T.Text -> Parser Namelist
namelistOfType nmlName = do
        (name, parameters) <- between (char '&') (char '/') namelistContent
        comments <- manyTill anyChar (lookAhead ((try (do; eol; spaces; char '&'; return ())) <|> eof))
        spaces
        if name == nmlName
            then return (Namelist name (T.pack comments) parameters)
            else namelistOfType nmlName

namelistContent :: Parser (T.Text, Map T.Text ParameterValue)
namelistContent = do
    name <- count 4 letter  -- TODO: is the definition really 4 letters
    spaces
    parameters <- many parameter
    let parameterMap = foldl' (\acc (k,v)-> M.insertWith combine k v acc) M.empty parameters
    return (T.pack name, parameterMap)
    where
        combine
            (ParArray arry1)
            (ParArray arry2)
            = ParArray (arry1 A.// (A.assocs arry2))
        combine _ _ = error "Parameter entered twice"


parameter :: Parser (T.Text, ParameterValue)
parameter = do
    name <- paramName
    pos <- optionMaybe paramPos
    spaces
    char '='
    spaces
    values <- sepValList paramValue
    let value = case values of
            [v] -> v
            vs -> case pos of
                    Nothing -> ParArray $ buildArray (Range 1 (length values), Single 1) values
                    Just posVals -> ParArray $ buildArray posVals values
    try ((do; spaces; char ','; spaces;) :: Parser ()) <|> spaces
    return $ (T.pack name, value)
    <?> "parameter"

-- |Create an array given a set of Ranges and the values.
buildArray :: (Range, Range) -> [ParameterValue] -> NamelistArray
buildArray ranges values =
    let
        indices = buildArrayIndices ranges
        (xs, ys) = unzip indices
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
    in A.array ((minX,minY),(maxX,maxY)) $ zip indices values

-- |Turn a set of Ranges into a list of indices.
buildArrayIndices :: (Range, Range) -> [(Int, Int)]
buildArrayIndices ((Single x),(Single y)) = [(x,y)]
buildArrayIndices ((Single x),(Range y1 y2)) = zip (repeat x) [y1..y2]
buildArrayIndices ((Range x1 x2), (Single y)) = zip [x1..x2] (repeat y)
buildArrayIndices ((Range _ _), (Range _ _)) = error "two dimensional ranges not supported"

sepValList :: (Parser ParameterValue) -> Parser [ParameterValue]
sepValList parser = many $ do
            n <- (try parser)
            (try $ do; spaces; optional $ char ','; spaces;)
            return n

paramName = do
    initialChar <- noneOf "=/( \n\t0123456789.,"
    remainingChars <- many (noneOf "=/( \n\t.,")
    return $ initialChar:remainingChars
    <?> "parameter name"
-- paramName = many1 (alphaNum)

paramPos :: Parser (Range, Range)
paramPos = between (char '(') (char ')') $ do
    r1 <- parseRange
    r2 <- optionMaybe $ do
        spaces
        char ','
        spaces
        parseRange
    return $ case r2 of
        Nothing -> (r1, Single 1)
        Just x -> (r1, x)

-- |Parses values in the form of "x" or "x:y" where x and y are integers, e.g. "1:2"
parseRange :: Parser Range
parseRange = do
    val1 <- intNumN
    val2 <- optionMaybe $ do
        char ':'
        intNumN
    return $ case val2 of
        Nothing -> Single val1
        Just x -> Range val1 x

data Range = Single Int | Range Int Int

paramValue :: Parser ParameterValue
paramValue =
    quotedString
    <|> try number
    <|> try boolean
    <?> "parameter value"

boolean :: Parser ParameterValue
boolean = do
    cs <- do
            try (string ".TRUE.")
            <|> string ".FALSE."
    return $ case cs of
        ".TRUE." -> ParBool True
        ".FALSE." -> ParBool False

quotedString :: Parser ParameterValue
quotedString = do
    char '\''
    result <- quotedContent
    char '\''
    return $ ParString $ T.pack result


quotedContent = many quotedChar

quotedChar = noneOf "\'"

intNumN :: Parser Int
intNumN = do
    sign <- optionMaybe (oneOf "-+")
    digits <- many1 (oneOf "0123456789-+")
    let num = case sign of
                Just s -> s:digits
                Nothing -> digits
    return $ read num
    <?> "int"

number :: Parser ParameterValue
number = do
    sign' <- optionMaybe $ oneOf "-+"
    let sign = case sign' of
            Just '+' -> []
            Just x -> [x]
            Nothing -> []
    num' <- many1 (oneOf "0123456789-+Ee.")
    if num' == "." then fail $ show num' ++ " is not a number" else return ()
    let num
            | head num' == '.' = sign ++ ('0':num')
            | last num' == '.' = sign ++ (num'++"0")
            | otherwise = sign ++ num'
    case readIntMaybe num of
        Just x -> return $ ParInt x
        Nothing -> case readFloatMaybe num of
            Just x -> return $ ParDouble x
            Nothing -> fail "not a number"

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

instance Show ParameterValue where
    show (ParString s)      = "\'" <> show s <> "\'"
    show (ParDouble n)      = show n
    show (ParInt n)         = show n
    show (ParBool True)     = ".TRUE."
    show (ParBool False)    = ".FALSE."
    -- TODO: fix this printing
    show (ParArray arry)     = intercalate "," (map (show .snd) $ A.assocs arry)


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

