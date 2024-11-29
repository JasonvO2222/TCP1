module Calendar where

import ParseLib
import DateTime
import Control.Applicative ((<|>))


-- Exercise 6
data Calendar = Calendar { calprops :: [Calprop],
                           events :: [Event]     }
    deriving (Eq, Ord, Show)

data Calprop = Prodid String 
             | Version Float deriving (Eq, Ord, Show)

newtype Event = Event { eventprops :: [Eventprop] }
    deriving (Eq, Ord, Show)

-- different types of properties an event can have
data Eventprop
    = DTstamp DateTime 
    | DTstart DateTime
    | DTend DateTime
    | UID String
    | Description String
    | Summary String
    | Location String
    deriving (Eq, Ord, Show)

-- The data type Eventprop contains objects which also expect a value (string or datetime), 
-- we need a type only version so that we can compare whether two Eventprops are of the same type.
-- This is because when we use just DTstamp for example, it works lika a constructor and expects a DateTime, which doesnt allow us to compare.
-- We made a function detectPropType to convert these Eventprops to their type so we can identify them
-- Pattern matching obviously doesnt have this issue, it is just that in some contexts we couldnt use pattern matching
-- We used the same method for the Tokens further down
data EventPropType
    = T_DTstamp  
    | T_DTstart 
    | T_DTend 
    | T_UID 
    | T_Description 
    | T_Summary 
    | T_Location 
    deriving (Eq, Ord, Show)

detectPropType :: Eventprop -> EventPropType
detectPropType (DTstamp _) = T_DTstamp
detectPropType (DTstart _) = T_DTstart
detectPropType (DTend _) = T_DTend
detectPropType (UID _) = T_UID
detectPropType (Description _) = T_Description
detectPropType (Summary _) = T_Summary
detectPropType (Location _) = T_Location

-- Exercise 7
data Token 
    = BegCalendar 
    | EndCalendar 
    | BegEvent 
    | EndEvent 
    | TokStamp DateTime
    | TokStart DateTime
    | TokEnd DateTime
    | TokUID String
    | TokDesc String
    | TokSum String
    | TokLoc String
    | TokVer Float
    | TokPro String
    deriving (Eq, Ord, Show)

data TokenType
    = T_BegCalendar 
    | T_EndCalendar 
    | T_BegEvent 
    | T_EndEvent 
    | T_TokStamp
    | T_TokStart
    | T_TokEnd
    | T_TokUID
    | T_TokDesc
    | T_TokSum
    | T_TokLoc
    | T_TokVer
    | T_TokPro
    deriving (Eq, Ord, Show)

detectTokenType :: Token -> TokenType
detectTokenType BegCalendar  = T_BegCalendar
detectTokenType EndCalendar  = T_EndCalendar
detectTokenType BegEvent  = T_BegEvent
detectTokenType EndEvent  = T_EndEvent
detectTokenType (TokStamp _) = T_TokStamp
detectTokenType (TokStart _) = T_TokStart
detectTokenType (TokEnd _) = T_TokEnd
detectTokenType (TokUID _) = T_TokUID
detectTokenType (TokDesc _) = T_TokDesc
detectTokenType (TokSum _) = T_TokSum
detectTokenType (TokLoc _) = T_TokLoc
detectTokenType (TokVer _) = T_TokVer
detectTokenType (TokPro _) = T_TokPro

-- Turning a a textual calendar to a list of tokens
lexCalendar :: Parser Char [Token]
lexCalendar = many (lexComp <|> lexProp)

-- Turning components into tokens, like begin calendar and begin vevent
lexComp :: Parser Char Token
lexComp = 
    (token "BEGIN:VCALENDAR" >> return BegCalendar)
    <|> 
    (token "\nEND:VCALENDAR\n" >> return EndCalendar)
    <|>
    (token "\nBEGIN:VEVENT" >> return BegEvent)
    <|>
    (token "\nEND:VEVENT" >> return EndEvent)


-- lexing event properties
lexProp :: Parser Char Token
lexProp =
    (token "\nDTSTAMP:" >>  (TokStamp <$> parseDateTime))
    <|>
    (token "\nDTSTART:" >>  (TokStart <$> parseDateTime))
    <|> 
    (token "\nDTEND:" >>  (TokEnd <$> parseDateTime))
    <|> 
    (token "\nUID:" >>  (TokUID <$> greedy takeSymbol))
    <|>
    (token "\nDESCRIPTION:" >>  (TokDesc <$> greedy takeSymbol))
    <|>
    (token "\nSUMMARY:" >>  (TokSum <$> greedy takeSymbol))
    <|>
    (token "\nLOCATION:" >>  (TokLoc <$> greedy takeSymbol))
    <|>
    (token "\nVERSION:" >> (TokVer <$> pF))
    <|>
    (token "\nPRODID:" >> (TokPro <$> greedy takeSymbol))

-- parsing version string to the corresponding float
-- example: "2.0" -> 2.0
pF :: Parser Char Float
pF = (\a b c -> fromIntegral a + fromIntegral c * 0.1) <$> pDig <*> symbol '.' <*> pDig


-- parse string to calender object
pCal :: Parser Char Calendar
pCal = (\a b c d -> Calendar b c) <$> token "BEGIN:VCALENDAR\n" <*>
                                      greedy pCalProp <*> 
                                      greedy pEvent <*> 
                                      token "END:VCALENDAR\n"


-- parsing string the calendar property, parses a Version or a Prodid
pCalProp :: Parser Char Calprop
pCalProp = pVersion <|> pProdid

-- Parsing entire 'version' string literal
pVersion :: Parser Char Calprop
pVersion = (\a b c -> b) <$> token "VERSION:" <*> pFloat <*> symbol '\n'

-- pVersion helper function to correctly parse the float in the version
pFloat :: Parser Char Calprop
pFloat = (\a b c -> Version (fromIntegral a + fromIntegral c * 0.1)) <$> pDig <*> symbol '.' <*> pDig

pProdid :: Parser Char Calprop
pProdid = (\a b c -> Prodid b) <$> token "PRODID:" <*> greedy takeSymbol <*> symbol '\n'

takeSymbol :: Parser Char Char
takeSymbol = (token "\n " >> takeSymbol) <<|> satisfy (/= '\n')

-- parse a single event from string, starts with a begin event, then greedily takes as many event properties as possible, then end event
pEvent :: Parser Char Event
pEvent = (\a b c -> Event b) <$> token "BEGIN:VEVENT\n" <*>
                                 greedy pEventProp <*> 
                                 token "END:VEVENT\n"

-- parsing event property
pEventProp :: Parser Char Eventprop
pEventProp = pDTstamp <|> pDTstart <|> pDTend <|> 
             pUID <|> pDescription <|> pSummary <|> pLocation

pDTstamp :: Parser Char Eventprop
pDTstamp =  (\a b c -> DTstamp b) <$> token "DTSTAMP:" <*> parseDateTime <*> symbol '\n'
pDTstart =  (\a b c -> DTstart b) <$> token "DTSTART:" <*> parseDateTime <*> symbol '\n'
pDTend =  (\a b c -> DTend b) <$> token "DTEND:" <*> parseDateTime <*> symbol '\n'
pUID =  (\a b c -> UID b) <$> token "UID:" <*> greedy takeSymbol <*> symbol '\n'
pDescription =  (\a b c -> Description b) <$> token "DESCRIPTION:" <*> greedy takeSymbol <*> symbol '\n'
pSummary =  (\a b c -> Summary b) <$> token "SUMMARY:" <*> greedy takeSymbol <*> symbol '\n'
pLocation =  (\a b c -> Location b) <$> token "LOCATION:" <*> greedy takeSymbol <*> symbol '\n'


-- parsing using tokens, creates a calendar object from tokens
parseCalendar :: Parser Token Calendar
parseCalendar = (\a b c d -> Calendar b c) <$> symbol BegCalendar <*> greedy pPropCal <*> greedy pEventToken <*> symbol EndCalendar


-- creating calendar properties, ie either the version or the prodid, from tokens
pPropCal :: Parser Token Calprop
pPropCal = pVersionTok <|> pProdidTok
    where
        pVersionTok :: Parser Token Calprop
        pVersionTok = (\(TokVer v) -> Version v) <$> satisfy (compareT T_TokVer)

        pProdidTok :: Parser Token Calprop 
        pProdidTok = (\(TokPro s) -> Prodid s) <$> satisfy (compareT T_TokPro)

-- comparing tokentypes to correctly parse
compareT :: TokenType -> Token -> Bool
compareT tt t = tt == (detectTokenType t)

-- parsing event from tokens
pEventToken :: Parser Token Event
pEventToken = (\_ props _ -> Event props) 
    <$> symbol BegEvent 
    <*> greedy pEventPropTok
    <*> symbol EndEvent

-- parsing an event property from a token, uses helper functions for individual properties
pEventPropTok :: Parser Token Eventprop
pEventPropTok = pDTstampTok <|> pDTstartTok <|> pDTendTok <|> pUIDTok <|> pDescTok <|> pSumTok <|> pLocTok

pDTstampTok :: Parser Token Eventprop
pDTstampTok = (\(TokStamp a) -> DTstamp a) <$> satisfy (compareT T_TokStamp)

pDTstartTok :: Parser Token Eventprop
pDTstartTok = (\(TokStart a) -> DTstart a) <$> satisfy (compareT T_TokStart)

pDTendTok :: Parser Token Eventprop
pDTendTok = (\(TokEnd a) -> DTend a) <$> satisfy (compareT T_TokEnd)

pUIDTok :: Parser Token Eventprop
pUIDTok = (\(TokUID a) -> UID a) <$> satisfy (compareT T_TokUID)

pDescTok :: Parser Token Eventprop
pDescTok = (\(TokDesc a) -> Description a) <$> satisfy (compareT T_TokDesc)

pSumTok :: Parser Token Eventprop
pSumTok = (\(TokSum a) -> Summary a) <$> satisfy (compareT T_TokSum)

pLocTok :: Parser Token Eventprop
pLocTok = (\(TokLoc a) -> Location a) <$> satisfy (compareT T_TokLoc)


recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
-- printing calendar object to a string 
printCalendar :: Calendar -> String
printCalendar (Calendar c e) = "BEGIN:VCALENDAR\n" ++ 
                               foldl (\a p -> a ++ printCalProp p) "" c ++ 
                               foldl (\a e -> a ++ printEvent e) "" e ++ 
                               "END:VCALENDAR\n"

printCalProp :: Calprop -> String
printCalProp p = case p of
    Prodid p -> "PRODID:" ++ show p ++ "\n"
    Version v -> "VERSION:" ++ show v ++ "\n"

printEvent :: Event -> String
printEvent (Event e) = "BEGIN:VEVENT\n" ++ 
                       foldl (\a p -> a ++ printEventProp p) "" e ++ 
                       "END:VEVENT\n"

printEventProp :: Eventprop -> String
printEventProp p = case p of
     DTstamp dt -> "DTSTART:" ++ show dt ++ "\n"
     DTstart dt -> "DTSTART:" ++ show dt ++ "\n"
     DTend dt -> "DTEND:" ++ show dt ++ "\n"
     UID str -> "UID:" ++ str ++ "\n"
     Description str -> "DESCRIPTION:" ++  str ++ "\n"
     Summary str -> "SUMMARY:" ++ str ++ "\n"
     Location str -> "LOCATION:" ++ str ++ "\n"