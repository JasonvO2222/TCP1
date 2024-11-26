module Calendar where

import ParseLib
import DateTime
import Control.Applicative ((<|>))


-- Exercise 6
data Calendar = Calendar { calprops :: [Calprop],
                           events :: [Event]     }
    deriving (Eq, Ord, Show)

data Calprop = CP_Prodid Prodid | CP_Version Version deriving (Eq, Ord, Show)

newtype Prodid = Prodid String deriving (Eq, Ord, Show)

newtype Version = Version Float deriving (Eq, Ord, Show)


newtype Event = Event { eventprops :: [Eventprop] }
    deriving (Eq, Ord, Show)

data Eventprop
    = EP_DTstamp DTstamp
    | EP_DTstart DTstart
    | EP_DTend DTend
    | EP_UID UID
    | EP_Description Description
    | EP_Summary Summary
    | EP_Location Location
    deriving (Eq, Ord, Show)

newtype DTstamp     = DTstamp DateTime  deriving (Eq, Ord, Show)
newtype DTstart     = DTstart DateTime  deriving (Eq, Ord, Show)
newtype DTend       = DTend DateTime  deriving (Eq, Ord, Show)
newtype UID         = UID String    deriving (Eq, Ord, Show)
newtype Description = Description String    deriving (Eq, Ord, Show)
newtype Summary     = Summary String deriving (Eq, Ord, Show)
newtype Location    = Location String deriving (Eq, Ord, Show)

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



-- ik denk eerst kijken of het een calender of n event is ofzo?
lexCalendar :: Parser Char [Token]
lexCalendar = many (lexComp <|> lexProp)

lexComp :: Parser Char Token
lexComp = 
    (token "BEGIN:VCALENDAR" >> return (BegCalendar))
    <|> 
    (token "\nEND:VCALENDAR\n" >> return (EndCalendar))
    <|>
    (token "\nBEGIN:VEVENT" >> return (BegEvent))
    <|>
    (token "\nEND:VEVENT" >> return (EndEvent))

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
pVersion = (\a b c -> CP_Version b) <$> token "VERSION:" <*> pFloat <*> symbol '\n'

pFloat :: Parser Char Version
pFloat = (\a b c -> Version (fromIntegral a + fromIntegral c * 0.1)) <$> pDig <*> symbol '.' <*> pDig

pProdid :: Parser Char Calprop
pProdid = (\a b c -> CP_Prodid (Prodid b)) <$> token "PRODID:" <*> greedy takeSymbol <*> symbol '\n'

takeSymbol :: Parser Char Char
takeSymbol = (const ' ') <$> token "\n " <<|> satisfy (/= '\n')

-- parse event
pEvent :: Parser Char Event
pEvent = (\a b c -> Event b) <$> token "BEGIN:VEVENT\n" <*>
                                 greedy pEventProp <*> 
                                 token "END:VEVENT\n"

pEventProp :: Parser Char Eventprop
pEventProp = pDTstamp <|> pDTstart <|> pDTend <|> 
             pUID <|> pDescription <|> pSummary <|> pLocation

pDTstamp :: Parser Char Eventprop
pDTstamp =  (\a b c -> EP_DTstamp (DTstamp b)) <$> token "DTSTAMP:" <*> parseDateTime <*> symbol '\n'
pDTstart =  (\a b c -> EP_DTstart (DTstart b)) <$> token "DTSTART:" <*> parseDateTime <*> symbol '\n'
pDTend =  (\a b c -> EP_DTend (DTend b)) <$> token "DTEND:" <*> parseDateTime <*> symbol '\n'
pUID =  (\a b c -> EP_UID (UID b)) <$> token "UID:" <*> greedy takeSymbol <*> symbol '\n'
pDescription =  (\a b c -> EP_Description (Description b)) <$> token "DESCRIPTION:" <*> greedy takeSymbol <*> symbol '\n'
pSummary =  (\a b c -> EP_Summary (Summary b)) <$> token "SUMMARY:" <*> greedy takeSymbol <*> symbol '\n'
pLocation =  (\a b c -> EP_Location (Location b)) <$> token "LOCATION:" <*> greedy takeSymbol <*> symbol '\n'


-- parse using token
parseCalendar :: Parser Token Calendar
parseCalendar = (\a b c d -> Calendar b c) <$> symbol BegCalendar <*> greedy pPropCal <*> greedy pEventToken <*> symbol EndCalendar

pPropCal :: Parser Token Calprop
pPropCal = pVersionTok <|> pProdidTok
    where
        pVersionTok :: Parser Token Calprop
        pVersionTok = (\(TokVer v) -> CP_Version (Version v)) <$> symbol (TokVer 0.0)

        pProdidTok :: Parser Token Calprop 
        pProdidTok = (\(TokPro s) -> CP_Prodid (Prodid s)) <$> symbol (TokPro "")  


pEventToken :: Parser Token Event
pEventToken = (\_ props _ -> Event props) 
    <$> symbol BegEvent 
    <*> greedy pEventPropTok
    <*> symbol EndEvent




pEventPropTok :: Parser Token Eventprop
pEventPropTok = pDTstampTok <|> pDTstartTok <|> pDTendTok <|> pUIDTok <|> pDescTok <|> pSumTok <|> pLocTok

pDTstampTok :: Parser Token Eventprop
pDTstampTok = (\(TokStamp a) -> EP_DTstamp (DTstamp a)) <$> symbol (TokStamp undefined)

pDTstartTok :: Parser Token Eventprop
pDTstartTok = (\(TokStart a) -> EP_DTstart (DTstart a)) <$> symbol (TokStart undefined)

pDTendTok :: Parser Token Eventprop
pDTendTok = (\(TokEnd a) -> EP_DTend (DTend a)) <$> symbol (TokEnd undefined)

pUIDTok :: Parser Token Eventprop
pUIDTok = (\(TokUID a) -> EP_UID (UID a)) <$> symbol (TokUID "")

pDescTok :: Parser Token Eventprop
pDescTok = (\(TokDesc a) -> EP_Description (Description a)) <$> symbol (TokDesc "")

pSumTok :: Parser Token Eventprop
pSumTok = (\(TokSum a) -> EP_Summary (Summary a)) <$> symbol (TokSum "")

pLocTok :: Parser Token Eventprop
pLocTok = (\(TokLoc a) -> EP_Location (Location a)) <$> symbol (TokLoc "")



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
    CP_Prodid p -> "PRODID:" ++ show p ++ "\n"
    CP_Version v -> "VERSION:" ++ show v ++ "\n"

printEvent :: Event -> String
printEvent (Event e) = "BEGIN:VEVENT\n" ++ 
                       foldl (\a p -> a ++ printEventProp p) "" e ++ 
                       "END:VEVENT\n"

printEventProp :: Eventprop -> String
printEventProp p = case p of
    EP_DTstamp (DTstamp dt) -> "DTSTART:" ++ show dt ++ "\n"
    EP_DTstart (DTstart dt) -> "DTSTART:" ++ show dt ++ "\n"
    EP_DTend (DTend dt) -> "DTEND:" ++ show dt ++ "\n"
    EP_UID (UID str) -> "UID:" ++ str ++ "\n"
    EP_Description (Description str) -> "DESCRIPTION:" ++  str ++ "\n"
    EP_Summary (Summary str) -> "SUMMARY:" ++ str ++ "\n"
    EP_Location (Location str) -> "LOCATION:" ++ str ++ "\n"