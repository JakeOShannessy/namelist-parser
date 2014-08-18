{-# LANGUAGE FlexibleInstances #-}
module Text.Namelist
    ( module Text.Namelist
    , module Text.Namelist.Types
    )
where

import qualified Data.Array as A
import Data.List
import Data.Map (Map(..))
import qualified Data.Map as M

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.Parsec.Language (haskellDef)
import Text.Read
import Text.Namelist.Types


readNml :: FilePath -> IO (Either ParseError NamelistFile)
readNml filepath = do
    rawText <- readFile filepath
    return $ parse nameListParser filepath rawText
            
            
parseNml :: String -> Either ParseError NamelistFile
parseNml input = parse nameListParser "(unknown)" input

nameListParser :: Parser NamelistFile
nameListParser = do
    headerComments <- many (noneOf "&")
    namelists <- many namelist
    eof
    return (NamelistFile headerComments namelists)
    

namelist :: Parser Namelist
namelist = do
        (name, parameters) <- between (char '&') (char '/') namelistContent
        comments <- many (noneOf "&")
        return (Namelist name comments parameters)

namelistContent :: Parser (String, Map String ParameterValue)
namelistContent = do
    name <- count 4 letter  -- TODO: is the definition really 4 letters
    spaces
    parameters <- many parameter
    let parameterMap = foldl' (\acc (k,v)-> M.insertWith combine k v acc) M.empty parameters
    return (name, parameterMap)
    where
        combine
            (ParArray arry1)
            (ParArray arry2)
            = ParArray (arry1 A.// (A.assocs arry2))
        combine _ _ = error "Parameter entered twice"
        

parameter :: Parser (String, ParameterValue)
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
    try (do; spaces; char ','; spaces;) <|> spaces
    return $ (name, value)
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
    
paramValue =
    quotedString
    <|> try number
    <|> try boolean
    <?> "parameter value"
    
boolean = do
    cs <- do
            try (string ".TRUE.")
            <|> string ".FALSE."
    return $ case cs of
        ".TRUE." -> ParBool True
        ".FALSE." -> ParBool False

quotedString = do
    char '\''
    result <- quotedContent
    char '\''
    return $ ParString result
    

quotedContent = many quotedChar

quotedChar = noneOf "\'\n"

intNumN :: Parser Int
intNumN = do
    sign <- optionMaybe (oneOf "-+")
    digits <- many1 (oneOf "0123456789-+")
    let num = case sign of
                Just s -> s:digits
                Nothing -> digits
    return $ read num
    <?> "int"
    

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
instance Show NamelistFile where
    show (NamelistFile comments nmls) = intercalate "" (map show nmls)
    
-- |Render a Namelist
instance Show Namelist where
    -- show (Namelist name comments []) = "&" ++ name ++ " " ++ "/" ++ comments ++ "\n"
    show (Namelist name comments parameters)
        = "&" ++ name ++ " "
        ++ (intercalate ",\n      " (renderParameters parameterList))
        ++ " /" ++ comments ++ "\n"
        where
            parameterList = M.toList parameters

-- |Render a list of Parameters
renderParameters :: [(String, ParameterValue)] -> [String]
renderParameters parameters = map renderParameter parameters
renderParameter :: (String, ParameterValue) -> String
renderParameter (name, value) = name ++ "=" ++  (show value)

-- |Render a Parameter
-- instance Show Parameter where
    -- show (Parameter name value) = name ++ "=" ++  (show value)
    -- --use intercalate to add commas to lists

-- |Convert the Haskell types into appropriate namelist strings using Show class    
instance Show ParameterValue where
    show (ParString s)      = "\'" ++ s ++ "\'"
    show (ParDouble n)      = show n
    show (ParInt n)         = show n
    show (ParBool True)     = ".TRUE."
    show (ParBool False)    = ".FALSE."
    -- TODO: fix this printing
    show (ParArray arry)     = intercalate "," (map show $ A.assocs arry)


-- |Create the text in namelist format
createNmlText :: NamelistFile -> String
createNmlText (NamelistFile comments nmlLists) = concat (map show nmlLists)

-- |Write namelist data to a namelist file
writeNml :: String -> NamelistFile -> IO ()
writeNml filename nmlFile = do
    writeFile filename (createNmlText nmlFile)

