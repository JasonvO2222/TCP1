module Calendar where

import ParseLib
import DateTime


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
data Token = Token deriving (Eq, Ord, Show)


-- ik denk eerst kijken of het een calender of n event is ofzo?
lexCalendar :: Parser Char [Token]
lexCalendar = undefined

-- parse calender
pCal :: Parser Char Calendar
pCal = undefined

-- parse event
pEvent :: Parser Char Event
pEvent = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

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
    EP_DTstamp dt -> "DTSTART:" ++ show dt ++ "\n"
    EP_DTstart dt -> "DTSTART:" ++ show dt ++ "\n"
    EP_DTend dt -> "DTEND:" ++ show dt ++ "\n"
    EP_UID str -> "UID:" ++ show str ++ "\n"
    EP_Description str -> "DESCRIPTION:" ++ show str ++ "\n"
    EP_Summary str -> "SUMMARY:" ++ show str ++ "\n"
    EP_Location str -> "LOCATION:" ++ show str ++ "\n"