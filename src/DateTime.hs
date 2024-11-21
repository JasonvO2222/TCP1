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
pTime = (\a b c d -> Time b c d) <$> pT <*> pHour <*> pMinute <*> pSecond

pT :: Parser Char Char
pT = symbol 'T'

pHour :: Parser Char Hour
pHour = (\a b -> Hour (a*10 + b)) <$> pDig <*> pDig

pMinute :: Parser Char Minute
pMinute = (\a b -> Minute (a*10 + b)) <$> pDig <*> pDig

pSecond :: Parser Char Second
pSecond = (\a b -> Second (a*10 + b)) <$> pDig <*> pDig

pUTC :: Parser Char Bool
pUTC = const True <$> satisfy isUTC <<|> const False <$> satisfy (const True)

isUTC :: Char -> Bool
isUTC c | c == 'Z' = True
        | otherwise = False

pDig :: Parser Char Int
pDig =  digitToInt <$> satisfy isDig

isDig :: Char -> Bool
isDig n = n `elem` ['0'..'9']

digitToInt :: Char -> Int
digitToInt c
    | c == '0' = 0
    | c == '1' = 1
    | c == '2' = 2
    | c == '3' = 3
    | c == '4' = 4
    | c == '5' = 5
    | c == '6' = 6
    | c == '7' = 7
    | c == '8' = 8
    | c == '9' = 9


-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run parser input = do
    let r = parse parser input
    if null r
    then Nothing
    else Just $ (fst . head) r -- [(r, [s])] -> r

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime d t u) = printDate d ++ "T" ++ printTime t ++ printUTC u

printUTC :: Bool -> String
printUTC False = []
printUTC True = "Z"

printDate :: Date -> String
printDate (Date y m d) = foldl (\a i-> a ++ show i) "" [runYear y, runMonth m, runDay d]

printTime :: Time -> String
printTime (Time h m s) = foldl (\a i -> a ++ show i) "" [runHour h, runMinute m, runSecond s]

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime d t u) = True

--check Date section
checkDate :: Date -> Bool
checkDate (Date y m d) = checkYear y && checkMonth m && checkDay y m d

checkYear :: Year -> Bool
checkYear y = let i = runYear y in (i >= 1000)

checkMonth :: Month -> Bool
checkMonth m = let i = runMonth m in (i >= 1 && i <= 12)

checkDay :: Year -> Month -> Day -> Bool
checkDay y m d = id >= 1 && id <= (helpDay leap im e)
    where leap = runYear y `mod` 4 == 0
          im = runMonth m
          e = even im
          id = runDay d

helpDay :: Bool -> Int -> Bool -> Int
helpDay True 2 _  = 29
helpDay _ 2 _     = 28
helpDay _ _ True  = 30
helpDay _ _ False = 31

--check Time section
checkTime :: Time -> Bool
checkTime (Time h m s) = checkHour h && checkMS (runMinute m) && checkMS (runSecond s)

checkHour :: Hour -> Bool
checkHour h = let i = runHour h in (i >= 0 && i <= 23)

checkMS :: Int -> Bool
checkMS i = i >= 0 && i <= 59


