import Data.Functor((<&>))
import Data.Ix (range)
import Data.List (break, group, maximumBy, sort, sortBy)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(todMin))

import qualified Data.Map.Strict as M

import DatedEvent

type GuardID = Int
type MinuteRange = (Int,Int)
type DatedEvent' = (LocalTime, Event)
type GuardSleepRecord = M.Map GuardID [MinuteRange]


toSleepRecords :: [DatedEvent'] -> GuardSleepRecord
toSleepRecords [] = M.empty
toSleepRecords ((_,(Guard guard')):rest) =
    case break (isGuard . snd) rest of

        ([], otherShifts) -> toSleepRecords otherShifts

        (sleepEvents,otherShifts) -> M.insertWith (++) guard' (toMinuteRanges sleepEvents) $ toSleepRecords otherShifts

    where isGuard e = case e of (Guard _) -> True
                                _         -> False

          toMinuteRanges :: [DatedEvent'] -> [MinuteRange]
          toMinuteRanges events = dropEveryOther staggeredPair
              where minutes = map (todMin . localTimeOfDay . fst) events
                    staggeredPair = zip minutes (tail minutes <&> minus 1)

toSleepRecords _ = error "Does not start with a guard"

findSleepiestGuard :: GuardSleepRecord -> GuardID
findSleepiestGuard = fst . maximumBy (onSecond compare) . M.toList . M.map sumMinuteRanges

findSleepiestMinute :: [MinuteRange] -> (Int, Int)
findSleepiestMinute l = (minute, numOccurrences) 
    where numOccurrences = length largestList
          minute         = head largestList
          largestList = maximumBy length' . group . sort . concat $ map range l
          length' a b = compare (length a) (length b)

main = do 
    events <- sortByFirst . map (getDatedEvent . read) . lines <$> getContents

    let sleepRecords = toSleepRecords events

    -- part A
    let sleepiestGuard = findSleepiestGuard sleepRecords
        theirSleepRecord = sleepRecords M.! sleepiestGuard
        sleepiestMinute = fst $ findSleepiestMinute theirSleepRecord

    print $ sleepiestGuard * sleepiestMinute

    -- part B
    let sleepiestMinutes = M.map findSleepiestMinute sleepRecords
        (reliableGuard, mostCommonMinute) = fmap fst
            . maximumBy (\(_,(_,n)) (_,(_,m)) -> compare n m) 
            $ M.toList sleepiestMinutes

    print $ reliableGuard * mostCommonMinute

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
sortByFirst :: (Ord a) => [(a,b)] -> [(a,b)]
sortByFirst = sortBy $ onFirst compare
