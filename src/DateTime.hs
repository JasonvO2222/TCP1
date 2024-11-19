module DateTime where

import ParseLib

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord)


-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> pDate <*> pTime <*> pUTC

--Date parse section
pDate :: Parser Char Date
pDate = Date <$> pYear <*> pMonth <*> pDay

pYear :: Parser Char Year
pYear = (\a b c d -> Year (a * 1000 + b * 100 + c * 10 + d) ) <$> pDig <*> pDig <*> pDig <*> pDig

pMonth :: Parser Char Month
pMonth = (\a b -> Month (a*10 + b) ) <$> pDig <*> pDig

pDay :: Parser Char Day
pDay = (\a b -> Day (a*10 + b) ) <$> pDig <*> pDig

--Time parse section
pTime :: Parser Char Time
pTime = undefined


pUTC :: Parser Char Bool
pUTC = undefined --(\x -> False) <$> <|> (\x -> True) <$> symbol 'Z'

pDig :: Parser Char Int
pDig =  digitToInt <$> satisfy isDig

isDig :: Char -> Bool
isDig = undefined

digitToInt :: Char -> Int
digitToInt = undefined

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run parser xs = do
    let r = parse parser xs
    if null r
    then Nothing
    else Just $ (fst . head) r

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = undefined

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
