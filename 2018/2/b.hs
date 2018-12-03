--import Data.List (group, nub, sort)

import Data.List (intersect)
import Data.Tuple (uncurry)
import Control.Monad (guard)

nearStrings :: [String] -> [(String,String)]
nearStrings strings = do
    x <- strings
    y <- tail $ dropWhile (/= x) strings
    let distance = length . filter (uncurry (/=)) $ zip x y
    guard $ distance < 2
    return (x,y)

main = do
    strings <-  lines <$> getContents
    print . map (uncurry intersect) $ nearStrings strings
