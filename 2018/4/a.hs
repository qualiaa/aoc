import Control.Monad (liftM)
import Data.Functor((<&>))
import Data.Ix (range)
import Data.List (break, find, group, groupBy, maximumBy, sort, sortBy)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(todMin))

import DatedEvent

type GuardID = Int
type MinuteRange = (Int,Int)
type DatedEvent' = (LocalTime, Event)
type GuardSleeps = (GuardID, [MinuteRange])

toMinuteRanges :: [DatedEvent'] -> [MinuteRange]
toMinuteRanges l = dropEveryOther staggeredPair
    where l' = map (todMin . localTimeOfDay . fst) l
          staggeredPair = zip l' (tail l' <&> minus 1)

extractRanges :: [DatedEvent'] -> [GuardSleeps]
extractRanges [] = []
extractRanges ((_,(Guard guard')):rest) =
    case break (isGuard . snd) rest of

        ([],otherShifts) -> extractRanges otherShifts

        (sleepEvents,otherShifts) -> (guard', toMinuteRanges sleepEvents) : extractRanges otherShifts

    where isGuard e = case e of (Guard _) -> True
                                _         -> False

extractRanges _ = error "Does not start with a guard"

-- feel like there's a nice monad for this but god damnit it we're long past that now
mergeRanges :: [GuardSleeps] -> [GuardSleeps]
mergeRanges = map concatSecond . groupByFirst . sortByFirst
    where concatSecond :: (Eq a) => [(a, [b])] -> (a, [b])
          concatSecond = foldl1 (\(a,x) (b,y) -> if a == b then (a, x++y) else error "uhuh")

findSleepiestGuard :: [GuardSleeps] -> GuardID
findSleepiestGuard = fst . maxBySecond . map (fmap sumMinuteRanges)

findSleepiestMinute :: [MinuteRange] -> (Int, Int)
findSleepiestMinute l = (minute, numOccurrences) 
    where numOccurrences = length largestList
          minute         = head largestList
          largestList = maximumBy length' . group . sort . concat $ map range l
          length' a b = compare (length a) (length b)
main = do 
    events <- sortByFirst . map (getDatedEvent . read) . lines <$> getContents

    let sleepTable = mergeRanges $ extractRanges events

    -- part A
    let sleepiestGuard = findSleepiestGuard sleepTable
        sleepiestMinuteRange = find ((== sleepiestGuard) . fst) sleepTable
        sleepiestMinute = liftM (fst . findSleepiestMinute . snd) sleepiestMinuteRange

    
    print $ (sleepiestGuard*) <$> sleepiestMinute

    -- part B
        
    -- (GuardID, (minute, numOccurrences)
    let sleepiestMinutes = map (fmap findSleepiestMinute) sleepTable
        mostCommonMinute = maximumBy (\(_,(_,n)) (_,(_,m)) -> compare n m) sleepiestMinutes
        guard = fst mostCommonMinute
        minute = fst $ snd mostCommonMinute

    print $ guard * minute
    --mapM putStrLn $ map (show . DatedEvent) events


-- utilities
numMinutes :: MinuteRange -> Int
numMinutes = uncurry minus

sumMinuteRanges :: [MinuteRange] -> Int
sumMinuteRanges = sum . map numMinutes 

dropEveryOther [] = []
dropEveryOther [x] = [x]
dropEveryOther (x:_:xs) = x:dropEveryOther xs

minus = flip (-)

-- oh how I wish for the day I learn arrows/lens
onFirst  f = \(a,_) (b,_) -> f a b
onSecond f = \(_,a) (_,b) -> f a b
maxBySecond = maximumBy $ onSecond compare
sortByFirst :: (Ord a) => [(a,b)] -> [(a,b)]
sortByFirst = sortBy $ onFirst compare
groupByFirst = groupBy $ onFirst (==)
