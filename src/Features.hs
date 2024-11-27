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
getStartTime e = let (DTstart dt : _) = eventprops e in dt

getEndTime :: Event -> DateTime
getEndTime e = let (DTend dt : _) = eventprops e in dt

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

-- huh?
timeSpent :: String -> Calendar -> Int
timeSpent = undefined

-- voor elke event met een bepaalde summary, tel minuten op


