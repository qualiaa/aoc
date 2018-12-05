module Patch
( Rect(..)
, Patch(..)
) where

import Control.Monad (mapM, mapM_, liftM)
import Text.Read (Read(readsPrec))
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

data Rect = Rect Int Int Int Int deriving (Show, Eq)
newtype Patch = Patch {getPatch :: (Int, Rect)} deriving (Show, Eq)

instance Read (Rect)  where readsPrec _ = readP_to_S readRect
instance Read (Patch) where readsPrec _ = readP_to_S readPatch

digit = satisfy isDigit
delim p c = skipSpaces >> (manyTill p $ char c)
intDelim :: (Integral a, Read a) => Char -> ReadP a
intDelim c = read <$> delim digit c

readRect :: ReadP Rect
readRect = do
    [x,y,w] <- mapM intDelim ",:x"
    h <- read <$> munch1 isDigit
    return $ Rect x y w h

readPatch :: ReadP Patch
readPatch = do
    id <- char '#' >> (intDelim ' ')
    string "@ "
    liftM (Patch . ((,) id)) readRect
