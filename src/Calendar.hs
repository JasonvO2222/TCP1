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

data Eventprop
    = DTstamp DateTime 
    | DTstart DateTime
    | DTend DateTime
    | UID String
    | Description String
    | Summary String
    | Location String
    deriving (Eq, Ord, Show)

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
    | TokStamp 
    | TokStart 
    | TokEnd 
    | TokUID 
    | TokDesc 
    | TokSum 
    | TokLoc 
    | TokVer 
    | TokPro 
    | TokNL
    | TokDT DateTime
    | TokFlt Float
    | TokTxt String
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
    | T_TokNL
    | T_TokDT
    | T_TokFlt
    | T_TokTxt
    deriving (Eq, Ord, Show)

detectTokenType :: Token -> TokenType
detectTokenType BegCalendar  = T_BegCalendar
detectTokenType EndCalendar  = T_EndCalendar
detectTokenType BegEvent  = T_BegEvent
detectTokenType EndEvent  = T_EndEvent
detectTokenType TokStamp = T_TokStamp
detectTokenType TokStart = T_TokStart
detectTokenType TokEnd = T_TokEnd
detectTokenType TokUID = T_TokUID
detectTokenType TokDesc = T_TokDesc
detectTokenType TokSum = T_TokSum
detectTokenType TokLoc = T_TokLoc
detectTokenType TokVer = T_TokVer
detectTokenType TokPro = T_TokPro
detectTokenType TokNL = T_TokNL
detectTokenType (TokDT _) = T_TokDT
detectTokenType (TokFlt _) = T_TokFlt
detectTokenType (TokTxt _) = T_TokTxt

-- ik denk eerst kijken of het een calender of n event is ofzo?
lexCalendar :: Parser Char [Token]
lexCalendar = many (lexComp <|> lexProp)

lexComp :: Parser Char Token
lexComp = 
    (token "BEGIN:VCALENDAR" >> return BegCalendar)
    <|> 
    (token "END:VCALENDAR" >> return EndCalendar)
    <|>
    (token "BEGIN:VEVENT" >> return BegEvent)
    <|>
    (token "END:VEVENT" >> return EndEvent)

lexProp :: Parser Char Token
lexProp =
    (token "DTSTAMP:" >> return TokStamp)
    <|>
    (token "DTSTART:" >> return TokStart)
    <|> 
    (token "DTEND:" >> return TokEnd)
    <|> 
    (token "UID:" >> return TokUID)
    <|>
    (token "DESCRIPTION:" >> return TokDesc)
    <|>
    (token "SUMMARY:" >> return TokSum)
    <|>
    (token "LOCATION:" >> return TokLoc)
    <|>
    (token "VERSION:" >> return TokVer)
    <|>
    (token "PRODID:" >> return TokPro)
    <|>
    (symbol '\n' >> return TokNL)
    <|>
    TokFlt <$> pF
    <|>
    TokDT <$> parseDateTime
    <|>
    TokTxt <$> greedy takeSymbol


pF :: Parser Char Float
pF = (\a b c -> fromIntegral a + fromIntegral c * 0.1) <$> pDig <*> symbol '.' <*> pDig


-- parse calender
pCal :: Parser Char Calendar
pCal = (\a b c d -> Calendar b c) <$> token "BEGIN:VCALENDAR\n" <*>
                                      greedy pCalProp <*> 
                                      greedy pEvent <*> 
                                      token "END:VCALENDAR\n"

pCalProp :: Parser Char Calprop
pCalProp = pVersion <|> pProdid

pVersion :: Parser Char Calprop
pVersion = (\a b c -> b) <$> token "VERSION:" <*> pFloat <*> symbol '\n'

pFloat :: Parser Char Calprop
pFloat = (\a b c -> Version (fromIntegral a + fromIntegral c * 0.1)) <$> pDig <*> symbol '.' <*> pDig

pProdid :: Parser Char Calprop
pProdid = (\a b c -> Prodid b) <$> token "PRODID:" <*> greedy takeSymbol <*> symbol '\n'

takeSymbol :: Parser Char Char
takeSymbol = (token "\n " >> takeSymbol) <<|> satisfy (/= '\n') 

-- parse event
pEvent :: Parser Char Event
pEvent = (\a b c -> Event b) <$> token "BEGIN:VEVENT\n" <*>
                                 greedy pEventProp <*> 
                                 token "END:VEVENT\n"

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


-- parse using token
parseCalendar :: Parser Token Calendar
parseCalendar = (\a b c d -> Calendar b c) <$> symbol BegCalendar <*> greedy pPropCal <*> greedy pEventToken <*> symbol EndCalendar

pPropCal :: Parser Token Calprop
pPropCal = pVersionTok <|> pProdidTok <|> (symbol TokNL >> pPropCal)
    where
        pVersionTok :: Parser Token Calprop
        pVersionTok =  (\a (TokFlt f) -> Version f) <$> symbol TokVer <*> satisfy (compareT T_TokFlt)

        pProdidTok :: Parser Token Calprop 
        pProdidTok = (\a (TokTxt t) -> Prodid t) <$> symbol TokPro <*> satisfy (compareT T_TokTxt)

compareT :: TokenType -> Token -> Bool
compareT tt t = tt == (detectTokenType t)

pEventToken :: Parser Token Event
pEventToken = (\_ props _ _-> Event props) <$> symbol BegEvent <*> greedy pEventPropTok <*> symbol EndEvent <*> symbol TokNL

pEventPropTok :: Parser Token Eventprop
pEventPropTok = pDTstampTok <|> pDTstartTok <|> pDTendTok <|> pUIDTok <|> pDescTok <|> pSumTok <|> pLocTok <|> (symbol TokNL >> pEventPropTok)

pDTstampTok :: Parser Token Eventprop
pDTstampTok = (\a (TokDT dt) -> DTstamp dt) <$> symbol TokStamp <*> satisfy (compareT T_TokDT)

pDTstartTok :: Parser Token Eventprop
pDTstartTok = (\a (TokDT dt) -> DTstart dt) <$> symbol TokStart <*> satisfy (compareT T_TokDT)

pDTendTok :: Parser Token Eventprop
pDTendTok = (\a (TokDT dt) -> DTend dt) <$> symbol TokEnd <*> satisfy (compareT T_TokDT)

pUIDTok :: Parser Token Eventprop
pUIDTok = (\a (TokTxt s) -> UID s) <$> symbol TokUID <*> satisfy (compareT T_TokTxt)

pDescTok :: Parser Token Eventprop
pDescTok = (\a (TokTxt s) -> Description s) <$> symbol TokDesc <*> satisfy (compareT T_TokTxt)

pSumTok :: Parser Token Eventprop
pSumTok = (\a (TokTxt s) -> Summary s) <$> symbol TokSum <*> satisfy (compareT T_TokTxt)

pLocTok :: Parser Token Eventprop
pLocTok = (\a (TokTxt s) -> Location s) <$> symbol TokLoc <*> satisfy (compareT T_TokTxt)


recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
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