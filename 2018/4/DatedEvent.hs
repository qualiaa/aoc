module DatedEvent
( Event(..)
, DatedEvent(..)
) where

import Control.Monad (liftM)
import Data.Char (isDigit)
import Data.Time.Calendar (fromGregorian, showGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Text.Read (Read(readsPrec))
import Text.ParserCombinators.ReadP

-- could use Date.Time.Format but that's cheating

newtype DatedEvent = DatedEvent {getDatedEvent :: (LocalTime, Event)}
data Event = Guard Int | WakeUp | Sleep

instance Show Event where
    show (Guard n) = "Guard #" ++ show n ++ " begins shift"
    show (WakeUp)  = "wakes up"
    show (Sleep)   = "falls asleep"

instance Show DatedEvent where
    show (DatedEvent ((LocalTime day time), e)) =
        "[" ++ showGregorian day ++ " " ++
               take 5 (show time) ++ "] " ++ show e

instance Read DatedEvent where readsPrec _ = readP_to_S readDatedEvent
instance Read Event      where readsPrec _ = readP_to_S readEvent

digit = satisfy isDigit

digitsThen n c = do s <- read <$> count n digit; char c; return s

delim p c = skipSpaces >> (manyTill p $ char c)

intDelim :: (Integral a, Read a) => Char -> ReadP a
intDelim c = read <$> delim digit c

readDate :: ReadP LocalTime
readDate = do
    y <- 4 `digitsThen` '-'
    m <- 2 `digitsThen` '-'
    d <- 2 `digitsThen` ' '
    h <- 2 `digitsThen` ':'
    min <- read <$> count 2 digit
    return (LocalTime (fromGregorian y m d) (TimeOfDay h min 0))

readNewGuard :: ReadP Event
readNewGuard = do
    n <- string "Guard #" >> intDelim ' '
    skipMany get >> eof
    return $ Guard n
    
readEvent :: ReadP Event
readEvent = do
    choice [readNewGuard, readWake, readSleep]
    where readWake  = string "wakes up"     >> return WakeUp
          readSleep = string "falls asleep" >> return Sleep

readDatedEvent :: ReadP DatedEvent
readDatedEvent = do
    date <- between (char '[') (char ']') readDate
    skipSpaces
    liftM (DatedEvent . (,) date) readEvent
