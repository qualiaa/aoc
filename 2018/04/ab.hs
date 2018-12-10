import Data.Ix (range)
import Data.List (break, group, maximumBy, sort)
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
                    staggeredPair = zip minutes (pred <$> tail minutes)

toSleepRecords _ = error "Does not start with a guard"

findSleepiestGuard :: GuardSleepRecord -> GuardID
findSleepiestGuard = fst . maximumBy (onSecond compare) . M.toList . M.map sumMinuteRanges

findCosiestMinute :: [MinuteRange] -> (Int, Int)
findCosiestMinute l = (minute, numOccurrences)
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
        cosiestMinute = fst $ findCosiestMinute theirSleepRecord

    print $ sleepiestGuard * cosiestMinute

    -- part B
    let cosiestMinutes = M.map findCosiestMinute sleepRecords
        (reliableGuard, theirFavouriteMinute) = fmap fst
            . maximumBy (\(_,(_,n)) (_,(_,m)) -> compare n m)
            $ M.toList cosiestMinutes

    print $ reliableGuard * theirFavouriteMinute

-- utilities
numMinutes :: MinuteRange -> Int
numMinutes = uncurry $ flip (-)

sumMinuteRanges :: [MinuteRange] -> Int
sumMinuteRanges = sum . map numMinutes

dropEveryOther [] = []
dropEveryOther [x] = [x]
dropEveryOther (x:_:xs) = x:dropEveryOther xs

-- oh how I wish for the day I learn arrows/lens
onSecond f = \(_,a) (_,b) -> f a b
sortByFirst = M.toAscList . M.fromList
