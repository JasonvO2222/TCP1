module Calendar where

import ParseLib
import DateTime


-- Exercise 6
data Calendar = Calendar { calprops :: [Calprop],
                           events :: [Event]     }
    deriving (Eq, Ord, Show)

data Calprop = Calprop deriving (Eq, Ord, Show)

newtype Prodid = Prodid   { prodid :: String } deriving (Eq, Ord, Show)
newtype Version = Version { version :: Float } deriving (Eq, Ord, Show)

data Event = Events { eventprops :: [Eventprop] }
    deriving (Eq, Ord, Show)

data Eventprop = Evenprop deriving (Eq, Ord, Show)

newtype DTstamp     = DTstamp     { dtstamp :: DateTime }   deriving (Eq, Ord)
newtype DTstart     = DTstart     { dtstart :: DateTime }   deriving (Eq, Ord)
newtype DTend       = DTend       { dtend :: DateTime }     deriving (Eq, Ord)
newtype UID         = UID         { uid :: String }         deriving (Eq, Ord, Show)
newtype Description = Description { description :: String } deriving (Eq, Ord, Show)
newtype Summary     = Summary     { summary :: String }     deriving (Eq, Ord, Show)
newtype Location    = Location    { location :: String }    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Token
    deriving (Eq, Ord, Show)

lexCalendar :: Parser Char [Token]
lexCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
