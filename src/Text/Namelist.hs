{-# LANGUAGE FlexibleInstances #-}
module Text.Namelist
    ( module Text.Namelist
    , module Text.Namelist.Types
    )
where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.Parsec.Language (haskellDef)

import Text.Namelist.Types
-- import Data.Array.IArray
-- import Control.Exception
import Data.List
import Data.Map (Map(..))
import qualified Data.Map as M
-- import qualified Data.Repa as R
import qualified Data.Vector as V
import Debug.Trace as D
--import System.Environment

--TODO: split the arrays into a different stage

-- readNml :: FilePath -> IO NamelistFile
readNml filepath = do
    rawText <- readFile filepath
    return $ case parse nameListParser filepath rawText of
            (Right nmlFile) -> Right (nmlFile)
            (Left err)      -> Left err
            
readNmlTest n filepath = do
    rawText <- readFile filepath
    return $ case parse nameListParser filepath (unlines $ take n $ lines rawText) of
            (Right nmlFile) -> Right (nmlFile)
            (Left err)      -> Left err
            
parseNml :: String -> Either ParseError NamelistFile
parseNml input = parse nameListParser "(unknown)" input

nameListParser :: Parser NamelistFile
nameListParser = do
    headerComments <- many (noneOf "&")   -- definition of comment wrong
    namelists <- many namelist
    eof
    return (NamelistFile namelists)
    

namelist :: Parser Namelist
namelist = do
        (name, parameters) <- (between (char '&') (char '/') namelistContent)
        comments <- many (noneOf "&")     -- definition of comment wrong
        return (Namelist name comments parameters)

-- TODO: amalgamation of arrays should occur here
namelistContent :: Parser (String, Map String ParameterValue)
namelistContent = do
    name <- count 4 letter
    spaces
    parameters <- many parameter
    let parameterMap = foldl' (\acc (k,v)-> M.insertWith combine k v acc) M.empty parameters
    return (name, parameterMap)
    where
        combine
            (ParameterArray ((origXMin,origYMin),(origXMax,origYMax)) origArrVals)
            (ParameterArray ((newXMin,newYMin),(newXMax,newYMax)) newArrVals)
            = undefined
        combine _ _ = error "Parameter entered twice"
        
sepByTest = do
    ls <- sepBy (try intNum) (char ',') -- (try $ do; spaces; optional $ char ','; spaces;)
    rem <- many anyChar
    return (ls, rem)

parameter :: Parser (String, ParameterValue)
parameter = do
    name <- paramName
    pos <- optionMaybe (between (char '(') (char ')') paramPos)
    spaces
    char '='
    spaces
    values <- sepValList paramValue
    let value = case values of
            [v] -> v
            vs -> case pos of
                    Nothing -> ParameterArray ((0,0),((length values)-1,0)) $ V.fromList values
                    Just posVals -> ParameterArray posVals $ V.fromList values
    spaces
    optional $ char ','
    spaces
    return $ (name, value)
    
sepValList parser = many $ do
            n <- (try parser)
            (try $ do; spaces; optional $ char ','; spaces;)
            return n
    
paramName = do
    initialChar <- noneOf "=/( \n\t0123456789."
    remainingChars <- many (noneOf "=/( \n\t.")
    return $ initialChar:remainingChars
    <?> "parameter name"
-- paramName = many1 (alphaNum)

paramPos :: Parser ((Int,Int),(Int,Int))
paramPos = do
    posVals <- sepBy1 (sepBy1 intNumN (char ':')) (char ',')
    return $ pos2Ind (map (map fromIntegral) posVals)

paramValue =
    quotedString
    <|> try number
    <|> boolean
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
    
-- array :: Maybe ((Int,Int),(Int,Int)) -> Parser ParameterValue
-- array pos = do -- TODO: should not be endBy, end comma not required
    -- values <- sepBy1 (paramValue) (do; spaces; char ','; spaces;)
    -- case pos of
        -- Nothing -> if length values <= 1
            -- then fail "insufficient number of values for list"
            -- else return $ ParameterArray ((0,0),((length values)-1,0)) $ V.fromList values
        -- Just posVals -> return $ ParameterArray posVals $ V.fromList values

quotedContent = many quotedChar

quotedChar = noneOf "\'\n"

pos2Ind :: [[Int]] -> ((Int,Int),(Int,Int))
pos2Ind (x:[]) = createRange x [1]
pos2Ind (x:xs) = createRange x (head xs)

createRange :: [Int] -> [Int] -> ((Int,Int),(Int,Int))
createRange x n | (length x == 2) && (length n == 1) = let a = head x  :: Int
                                                           b = head $ tail x  :: Int
                                                           n' = head n  :: Int
                                                       in ((a,n'),(b,n'))
                | (length x == 1) && (length n == 2) = let a = head n :: Int
                                                           b = head $ tail n  :: Int
                                                           x' = head x  :: Int
                                                       in ((x',a),(x',b))
                | (length x == 1) && (length n == 1) = let x' = head x :: Int
                                                           n' = head n  :: Int
                                                       in ((x',n'),(x',n'))

lexer = makeTokenParser haskellDef
fInteger = decimal lexer
-- fDouble = float lexer
natOrFloat = naturalOrFloat lexer

intNum :: Parser ParameterValue
intNum = do
    sign' <- optionMaybe $ oneOf "-+"
    let sign = case sign' of
            Just '+' -> []
            Just x -> [x]
            Nothing -> []
    digits <- many1 (oneOf "0123456789")
    dot <- optionMaybe (char '.')
    case dot of
        Just _ -> fail "not int"
        Nothing -> return ()
    let num = sign ++ digits
    return $ ParInt $ read num
    <?> "int"
    
intNumN :: Parser Int
intNumN = do
    sign' <- optionMaybe $ oneOf "-+"
    let sign = case sign' of
            Just '+' -> []
            Just x -> [x]
            Nothing -> []
    digits <- many1 (oneOf "0123456789")
    dot <- optionMaybe (char '.')
    case dot of
        Just _ -> fail "not int"
        Nothing -> return ()
    let num = sign ++ digits
    return $ read num
    <?> "int"
    
floatNum :: Parser ParameterValue
floatNum = do
    sign' <- optionMaybe $ oneOf "-+"
    let sign = case sign' of
            Just '+' -> []
            Just x -> [x]
            Nothing -> []
    digits <- many1 (oneOf "0123456789")
    exp' <- optionMaybe $ exponentParse
    let exp = case exp' of
            Just x -> x
            Nothing -> []
    let num = sign++digits++exp
    return $ ParDouble $ read num
    <?> "float"
    
exponentParse = do
    e <- many (oneOf "Ee")
    sign' <- optionMaybe $ oneOf "-+"
    let sign = case sign' of
            Just '+' -> []
            Just x -> [x]
            Nothing -> []
    digits1 <- many (oneOf "0123456789-+Ee.")
    dot' <- optionMaybe $ char '.'
    let dot = case dot' of
            Just x -> [x]
            Nothing -> []
    digits2' <- optionMaybe $ many (oneOf "0123456789-+Ee.")
    let digits2 = case digits2' of
            Just x -> x
            Nothing -> []
    return $ e ++ sign ++ digits1 ++ dot ++ digits2
    
number
    =   (try intNum)
    <|> floatNum

-- number = do
    -- sign <- optionMaybe (char '-')
    -- cs <- natOrFloat
    -- return $ case sign of
        -- Just '-' -> case cs of
                      -- Left n -> ParInt $ fromInteger (-n)
                      -- Right n -> ParDouble (-n)
        -- Nothing  -> case cs of
                      -- Left n -> ParInt $ fromInteger n
                      -- Right n -> ParDouble n

fInteger' = do
    cs <- fInteger
    return $ ParInt (fromIntegral cs)


        
-- class ParameterValue a
-- instance ParameterValue [Char]
-- instance ParameterValue Double
-- instance ParameterValue Bool
-- instance (ParameterValue a) => ParameterValue (ParameterArray a)

-- -- TODO: double check this specification of namelist arrays
-- -- note that these arrays may only be part of an array.
-- data (ParameterValue a) => ParameterArray a = ParameterArray ((Int,Int),(Int,Int)) (V.Vector a)

-- OUT
-- |Render a Namelist
instance Show NamelistFile where
    show (NamelistFile nmls) = intercalate "" (map show nmls)
    
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
    show (ParameterArray pos arry)     = intercalate "," (V.toList $ V.map show arry)
        -- where list = extractLong (array2dList arry)
    -- show (ParArry arry)     = intercalate "," (map show list)
        -- where list = extractLong (array2dList arry)
    -- show (ParArryPart pos arry) = show pos ++ show arry
    -- show ArrayEmpty         = "OHMG AN EMPTY ELEMENT"

-- extractLong :: [[a]] -> [a]
-- extractLong [x] = x
-- extractLong xs | length ys == 1 = head ys
    -- where ys = transpose xs
-- array2dList :: (Array (Int,Int) ParameterValue) -> [[ParameterValue]]
-- array2dList array = map (filter ifArrayNotEmpty) $ divArr cols elms
    -- where bnds = snd (bounds array)
          -- cols = snd bnds
          -- elms = elems array
-- divArr :: Int -> [ParameterValue] -> [[ParameterValue]]
-- divArr _ [] = []
-- divArr cols elms = take cols elms : divArr cols (drop cols elms)
-- ifArrayNotEmpty :: ParameterValue -> Bool
-- ifArrayNotEmpty x = case x of
    -- ArrayEmpty -> False
    -- _       -> True
-- ifArrayNotEmptyP :: ParameterValue -> Bool
-- ifArrayNotEmptyP x = case x of
    -- ArrayEmpty -> False
    -- _       -> True

-- |Create the text in namelist format
createNmlText :: NamelistFile -> String
createNmlText (NamelistFile nmlLists) = concat (map show nmlLists)

-- |Write namelist data to a namelist file
writeNml :: String -> NamelistFile -> IO ()
writeNml filename nmlFile = do
    writeFile filename (createNmlText nmlFile)

