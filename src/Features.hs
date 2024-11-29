module Features where

import DateTime
import Calendar


-- Exercise 9
-- Amount of events in a calender
countEvents :: Calendar -> Int
countEvents c = length (events c)

-- Finding events, uses helper function to see if event is within bounds
findEvents :: DateTime -> Calendar -> [Event]
findEvents dt c = [x | x <- (events c), getStartTime x <= dt && getEndTime x > dt]

getStartTime :: Event -> DateTime
getStartTime (Event es) = (\(DTstart dt) -> dt) (getDT es T_DTstart)

getEndTime :: Event -> DateTime
getEndTime (Event es) = (\(DTend dt) -> dt) (getDT es T_DTend)

-- looking for either start or end time based on the eventprop type
getDT :: [Eventprop] -> EventPropType -> Eventprop
getDT (p:ps) t | (detectPropType p) == t = p
               | otherwise = getDT ps t 

-- Check if any events overlap
checkOverlapping :: Calendar -> Bool
checkOverlapping c = any (uncurry isOverlap) pairlist
    where
        evs = events c
        pairlist = [(x,y) | x <- evs, y <- evs, x /= y]

-- Check if two events overlap
isOverlap :: Event -> Event -> Bool
isOverlap e1 e2 = (e1start < e2end && e1end > e2start)
    where
        e1start = getStartTime e1
        e1end   = getEndTime e1
        e2start = getStartTime e2
        e2end   = getEndTime e2



-- Calculate time spent on all events with a given summary
timeSpent :: String -> Calendar -> Int
timeSpent s (Calendar cps eps) = sum (map getDuration (iterateEvents eps s))

-- Duration of a single event
getDuration :: Event -> Int
getDuration e = dateDiff startD endD + timeDiff startT endT
      where (DateTime startD startT sb) = getStartTime e
            (DateTime endD endT se) = getEndTime e

-- Difference between 2 times
timeDiff :: Time -> Time -> Int
timeDiff s@(Time ah am as) e@(Time bh bm bs) = (runHour bh * 3600 + runMinute bm * 60 + runSecond bs) - (runHour ah * 3600 + runMinute am * 60 + runSecond as)

-- uses traverseDays to calculate amount of days between dates
dateDiff :: Date -> Date -> Int
dateDiff s e = (traverseDays 0 s e) * 86400

-- same date -> return int, otherwise -> 1+ & recurse
traverseDays :: Int -> Date -> Date -> Int
traverseDays acc s e | s == e = acc
                     | otherwise = traverseDays (acc + 1) (nextDay s) e

-- gives next date based on a given date, handles edge cases like leap years 
nextDay :: Date -> Date
nextDay (Date y m d) | (runDay d) < daysInMonth = Date y m (Day (runDay d + 1))
                     | (runMonth m) < 12 = Date y (Month (runMonth m + 1)) (Day 0)
                     | otherwise = Date (Year (runYear y + 1)) (Month 0) (Day 0)
               where leap = runYear y `mod` 4 == 0
                     daysInMonth = let im = runMonth m in helpDay leap im (even im)

-- go over all events and compare their summary to the given summary
iterateEvents :: [Event] -> String -> [Event]
iterateEvents es s = filter (compareSummary s) es

compareSummary :: String -> Event -> Bool
compareSummary s (Event props) = (getSummary props) == s

-- self explanatory, retrieves summary from a list of evenr properties
getSummary :: [Eventprop] -> String
getSummary [] = ""
getSummary (p:ps) | (detectPropType p) == T_Summary = (\(Summary s) -> s) p
               | otherwise = getSummary ps


