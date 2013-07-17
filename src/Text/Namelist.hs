module Text.Namelist (
    readNml, createNmlText, writeNml, Namelist(Namelist), NamelistFile(NamelistFile),
    Parameter(Parameter), ParameterValue(ParString,ParDouble,ParInt,ParBool,ParArry,ParArryPart,ArrayEmpty), mkParArray
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.Parsec.Language (haskellDef)
import Data.Array.IArray
import Data.List
--import System.Environment

--TODO: split the arrays into a different stage

--readNml :: FilePath -> IO NamelistFile
readNml file = do
    s <- readFile file
    let res = parse nameListFile file s
        res2 = case res of
            (Right nmlFile) -> Right (mergeAllNmlArr nmlFile)
            (Left err)      -> Left err
    return res2

nameListFile :: GenParser Char st NamelistFile
nameListFile = do
    comments <- many (noneOf "&")   -- definition of comment wrong
    cs <- many nml
    eof
    return (NamelistFile cs)
    


nml = do
        (name, params) <- (between (char '&') (char '/') pars)
        comments <- many (noneOf "&")     -- definition of comment wrong
        return (Namelist name comments params)

pars = do
    name <- count 4 letter
    paramStrings <- parameter
    return (name, paramStrings)

parameter = do
    skipMany (oneOf "\n ")
    many splitPar

splitPar = do
    paramName <- many1 (noneOf "=/( \n\t")
    pos <- optionMaybe (between (char '(') (char ')') parsePos)
    skipMany (oneOf "\n\t ")
    char '='
    skipMany (oneOf "\n\t ")
    paramValues <- sepEndBy parV (char ',')
    skipMany (oneOf "\n\t ")
--    let pV = case pos of
--              Just bb -> ParArryPart bb paramValues
--              Nothing -> if (length paramValues) == 1 then (head paramValues)
--                               else ParList  paramValues
    ---- The below is for proper arrays, but can leave some elements unbound, a design problem
    let pV = case pos of
              Just bb -> ParArryPart bb paramValues
              Nothing -> if (length paramValues) == 1 then (head paramValues)
                               else ParArryPart ((1,1),((length paramValues),1)) paramValues
    return (Parameter paramName pV)

mkArray' :: (Ix i) => (i,i) -> [ParameterValue] -> Array i ParameterValue
mkArray' poses las = array poses (toArr poses las)
toArr :: (Ix i) => (i,i) -> [a] -> [(i,a)]
toArr poses ls = zip (range poses) ls

createPos ls = "1:" ++ (show (length ls))

-- check rangeSize
createAssoc (ParArryPart dims list) = zip (range dims) list

--TODO: check that all params have the same name
combineArrParameters :: [Parameter] -> (String,[((Int,Int),ParameterValue)])
--combineArrParameters [] = (,)
combineArrParameters ((Parameter name value):ps) = (name, (combineArrParts (value : combineArrParameters' ps)))

combineArrParameters' [] = []
combineArrParameters' ((Parameter name value):ps) = value : combineArrParameters' ps

combineArrParts :: [ParameterValue] -> [((Int,Int),ParameterValue)]
combineArrParts [] = []
combineArrParts (arr:arrParts) = (createAssoc arr) ++ (combineArrParts arrParts)

mkArray :: (String,[((Int,Int),ParameterValue)]) -> Parameter
mkArray (name, assocs) = Parameter name (ParArry (mkParArray assocs))
mkParArray :: [((Int,Int),ParameterValue)] -> (Array (Int,Int) ParameterValue)
mkParArray assocs = assoc2Array assocs

assoc2Array :: [((Int,Int),ParameterValue)] -> Array (Int,Int) ParameterValue
assoc2Array assocs = accumArray (\a b -> b) ArrayEmpty (arrBounds assocs) assocs

--TODO: Consider getting rid of ArrayEmpty and managing the problem by other means

arrBounds assocs = (\(x,y) -> ((minimum x, minimum y),(maximum x, maximum y))) $ unzip $ map (\(pos,_) -> pos) assocs

findParArr ((Parameter pName (ParArryPart dims lst)):ps) =  (Parameter pName (ParArryPart dims lst)) : (findParArr ps)
findParArr ((Parameter _ _):ps) = findParArr ps
findParArr [] = []

mergeAllNmlArr (NamelistFile nmlList) = NamelistFile (mergeAllNmlArr' nmlList)
mergeAllNmlArr' [] = []
mergeAllNmlArr' (nml:nmlList) = (mergeNmlArr nml) : (mergeAllNmlArr' nmlList)

mergeNmlArr nml = Namelist nmlName comments ((remOldArr nmlParams) ++ newArrParams)
    where
        (Namelist nmlName comments nmlParams) = nml
        newArrParams = map mkArray $ map combineArrParameters $ splitCommonArr $ (findParArr nmlParams)

remOldArr :: [Parameter] -> [Parameter]
remOldArr ((Parameter _ (ParArryPart _ _)):ps) = remOldArr ps
remOldArr (par:ps) = par:(remOldArr ps)
remOldArr [] = []

splitCommonArr :: [Parameter] -> [[Parameter]]
splitCommonArr [] = []
splitCommonArr ((Parameter pName value):[]) = [[Parameter pName value]]
splitCommonArr ((Parameter pName value):ps) = ((Parameter pName value) : (filter ((==) pName . (\(Parameter p _) -> p)) ps)) : (splitCommonArr (filter ((/=) pName . (\(Parameter q _) -> q)) ps))

parV = do
    quotedString
    <|> number
    <|> boolean

parsePos = do
    posVals <- sepBy1 (sepBy1 fInteger (char ':')) (char ',')
    return $ pos2Ind (map (map fromIntegral) posVals)

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
fDouble = float lexer
natOrFloat = naturalOrFloat lexer

number = do
    sign <- optionMaybe (char '-')
    cs <- natOrFloat
    return $ case sign of
        Just '-' -> case cs of
                      Left n -> ParInt $ fromInteger (-n)
                      Right n -> ParDouble (-n)
        Nothing  -> case cs of
                      Left n -> ParInt $ fromInteger n
                      Right n -> ParDouble n

fInteger' = do
    cs <- fInteger
    return $ ParInt (fromIntegral cs)


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

parseNml :: String -> Either ParseError NamelistFile
parseNml input = parse nameListFile "(unknown)" input

-- |Holds the entire Namelist file, which is basically a list of namelists
data NamelistFile = NamelistFile
    [Namelist]  -- ^The namelists which make up the file

-- |Holds the name and parameters for each namelist read in from namelist file
data Namelist = Namelist
    String
    String
    [Parameter] -- ^Name, comments and List of Parameters

-- |Holds the name and value of a parameter
data Parameter = Parameter
    String     
    ParameterValue  -- ^Name and Value

-- |The different types of parameter value
data ParameterValue =
        ParString String
        | ParDouble Double
        | ParInt Int
        | ParBool Bool
        | ParArryPart ((Int,Int),(Int,Int)) [ParameterValue]
        | ParArry (Array (Int,Int) ParameterValue)
        | ArrayEmpty



-- OUT
-- |Render a Namelist
instance Show NamelistFile where
    show (NamelistFile nmls) = intercalate "" (map show nmls)
    
-- |Render a Namelist
instance Show Namelist where
    show (Namelist name comments []) = "&" ++ name ++ " " ++ "/" ++ comments ++ "\n"
    show (Namelist name comments parameters) = "&" ++ name ++ " " ++ (intercalate ", " (renderParameters parameters)) ++ " /" ++ comments ++ "\n"

-- |Render a list of Parameters
renderParameters :: [Parameter] -> [String]
renderParameters parameters = map show parameters

-- |Render a Parameter
instance Show Parameter where
    show (Parameter name value) = name ++ "=" ++  (show value)
    --use intercalate to add commas to lists

-- |Convert the Haskell types into appropriate namelist strings using Show class    
instance Show ParameterValue where
    show (ParString s)      = "\'" ++ s ++ "\'"
    show (ParDouble n)      = show n
    show (ParInt n)         = show n
    show (ParBool True)     = ".TRUE."
    show (ParBool False)    = ".FALSE."
    show (ParArry arry)     = intercalate "," (map show list)
        where list = extractLong (array2dList arry)
    show (ParArryPart pos arry) = show pos ++ show arry
    show ArrayEmpty         = "OHMG AN EMPTY ELEMENT"

extractLong :: [[a]] -> [a]
extractLong [x] = x
extractLong xs | length ys == 1 = head ys
    where ys = transpose xs
array2dList :: (Array (Int,Int) ParameterValue) -> [[ParameterValue]]
array2dList array = map (filter ifArrayNotEmpty) $ divArr cols elms
    where bnds = snd (bounds array)
          cols = snd bnds
          elms = elems array
divArr :: Int -> [ParameterValue] -> [[ParameterValue]]
divArr _ [] = []
divArr cols elms = take cols elms : divArr cols (drop cols elms)
ifArrayNotEmpty :: ParameterValue -> Bool
ifArrayNotEmpty x = case x of
    ArrayEmpty -> False
    _       -> True
ifArrayNotEmptyP :: ParameterValue -> Bool
ifArrayNotEmptyP x = case x of
    ArrayEmpty -> False
    _       -> True

-- |Create the text in namelist format
createNmlText :: NamelistFile -> String
createNmlText (NamelistFile nmlLists) = concat (map show nmlLists)

-- |Write namelist data to a namelist file
writeNml :: String -> NamelistFile -> IO ()
writeNml filename nmlFile = do
    writeFile filename (createNmlText nmlFile)

