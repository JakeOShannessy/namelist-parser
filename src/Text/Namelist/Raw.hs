{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Text.Namelist.Raw
    ( module Text.Namelist.Raw
    , module Text.Namelist.Raw.Types
    )
where

import qualified Data.Array as A
import Data.List hiding (takeWhile)
import Data.Map (Map(..))
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char
import Data.Attoparsec.Text as P

-- import Text.Parsec
-- import Text.Parsec.Text as PT
-- import Text.Read
import Text.Namelist.Types

tester :: Parser a -> T.Text -> (Either String a)
tester parser string = parseOnly (do; r <- parser; endOfInput; return r;) string
    
readNml :: FilePath -> IO (Either String RawNamelistFile)
readNml filepath = do
    rawText <- T.readFile filepath
    return $ parseOnly nameListParser rawText
            
            
parseNml :: T.Text -> Either String RawNamelistFile
parseNml input = parseOnly nameListParser input

nameListParser :: Parser RawNamelistFile
nameListParser = do
    headerComments <- P.takeWhile (\c-> c /= '&')
    namelists <- many' namelist
    endOfInput
    return (RawNamelistFile (headerComments) namelists)
    
eol :: Parser ()
eol = do
    char '\r'
    option 'a' (char '\n')
    return ()
    <?> "end of line"

namelist :: Parser RawNamelist
namelist = do
    char '&'
    name <- takeWhile1 (\c-> isLetter c || c == '_')
    skipSpace
    parameters <- many' parameter
    char '/'
    comments <- P.takeWhile (\c -> c /= '&') -- TODO: what about ampersands in comments
    skipSpace
      --  (name, parameters) <- between (char '&') (char '/') namelistContent
      --  comments <- manyTill anyChar (lookAhead ((try (do; eol; skipSpace; char '&'; return ())) <|> eof))
      --  skipSpace
    return (RawNamelist name comments parameters)

parameter :: Parser RawParameter
parameter = do
    -- take the parameter name, all valid letters up to a space or equals sign
    name <- paramName
    skipSpace -- TODO: check this skipSpace
    -- there maybe some position information following the parameter name which
    -- gives position values. This is in parenthesis
    pos <- option Nothing (do; pos <- paramPos; return $ Just pos;)
    skipSpace
    char '='
    skipSpace
    values <- paramValue `sepBy1'` valueSeparator
    valueSeparator
    return $ RawParameter name pos values
    <?> "parameter"
    
paramName = takeWhile1 (\c-> isLetter c || isDigit c || c == '_')
    <?> "parameter name"
    
           
valueSeparator = do
    skipSpace
    option ',' (char ',')
    skipSpace

paramPos :: Parser Position
paramPos = do
    char '('
    r1 <- parseRange
    r2 <- option Nothing $ do
        skipSpace
        char ','
        skipSpace
        rr <- parseRange
        return $ Just rr
    char ')'
    return $ case r2 of
        Nothing -> Position1 r1
        Just x -> Position2 r1 x
    
-- |Parses values in the form of "x" or "x:y" where x and y are integers, e.g. "1:2"
parseRange :: Parser Range
parseRange = do
    val1 <- decimal
    val2 <- option Nothing $ do
        char ':'
        n <- decimal
        return $ Just n
    return $ case val2 of
        Nothing -> Single val1
        Just x -> Interval val1 x
    

    
paramValue :: Parser ParameterValue
paramValue = choice
    [ quotedString
    , try paramNumber
    , try boolean
    ]

boolean :: Parser ParameterValue
boolean = do
    cs <- choice
        [ try (string ".TRUE.")
        , try (string ".FALSE.")
        ]
    return $ case cs of
        ".TRUE." -> ParBool True
        ".FALSE." -> ParBool False

quotedString :: Parser ParameterValue
quotedString = do
    qm <- satisfy (\c-> c == '\'' || c == '"')
    result <- quotedContent
    char qm
    return $ ParString $  result
    

quotedContent = P.takeWhile quotedChar

quotedChar c = c /= '\'' && c /= '\"'
eolTest c = c == '\r' || c == '\n'

   
paramNumber :: Parser ParameterValue
paramNumber = do
    n <- signed double -- TODO: parse integers if possible
    return $ ParDouble n


-- OUT
-- |Render a Namelist
instance PPrint RawNamelistFile where
    pprint (RawNamelistFile comments nmls) = T.intercalate "" (map pprint nmls)
    
-- |Render a Namelist
instance PPrint RawNamelist where
    -- show (Namelist name comments []) = "&" ++ name ++ " " ++ "/" ++ comments ++ "\n"
    pprint (RawNamelist name comments parameters)
        = "&" <> name <> " "
        <> (T.intercalate ",\n      " (map pprint parameters))
        <> " /" <> comments <> "\n"
    
class PPrint a where
    pprint :: a -> T.Text

instance PPrint RawParameter where
    pprint (RawParameter name posMaybe values) = name <> posStr <> "="
        <> T.intercalate "," (map pprint values)
        where
            posStr = case posMaybe of
                Nothing -> ""
                Just pos -> pprint pos
instance PPrint ParameterValue where
    pprint (ParString s)      = "\'" <> s <> "\'"
    pprint (ParDouble n)      = pprint n
    pprint (ParInt n)         = pprint n
    pprint (ParBool True)     = ".TRUE."
    pprint (ParBool False)    = ".FALSE."
    
instance PPrint Position where
    pprint (Position1 r)     = "(" <> pprint r <> ")" 
    pprint (Position2 r1 r2) = "(" <> pprint r1 <> "," <> pprint r2 <> ")"
 
instance PPrint Range where
    pprint (Single i)       = pprint i
    pprint (Interval i1 i2) = pprint i1 <> ":" <> pprint i2
   
instance PPrint a => PPrint [a] where
    pprint ls = "[" <> T.intercalate "," (map pprint ls) <> "]"
    
instance PPrint Double where
    pprint d = T.pack $ show d
    
instance PPrint Int where
    pprint d = T.pack $ show d

-- |Create the text in namelist format
createNmlText :: RawNamelistFile -> T.Text
createNmlText (RawNamelistFile comments nmlLists) = T.concat (map pprint nmlLists)

-- |Write namelist data to a namelist file
writeNml :: FilePath -> RawNamelistFile -> IO ()
writeNml filename nmlFile = do
    T.writeFile filename (createNmlText nmlFile)

