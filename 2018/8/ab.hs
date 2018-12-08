import Control.Monad
import Data.Char(isDigit)
import Data.Maybe(catMaybes)
import Data.List(intercalate)
import Text.Read(Read(readsPrec))
import Text.ParserCombinators.ReadP

data Tree = Node [Tree] [Int] deriving (Show)

instance Read Tree where
    readsPrec _ = readP_to_S readNode

readInt :: ReadP Int
readInt = read <$> (skipSpaces >> munch1 isDigit)

readHeader :: ReadP (Int, Int)
readHeader = liftM (,) readInt `ap` readInt

readMetaData :: Int -> ReadP [Int]
readMetaData m = count m readInt

readNode :: ReadP Tree
readNode = do
    (n, m) <- readHeader
    liftM2 Node (count n readNode) (readMetaData m)
    
sumTree :: Tree -> Int
sumTree (Node ns xs) = foldl (\a n -> a + sumTree n) 0 ns + (sum xs)

ix :: [a] -> Int -> Maybe a
ix l i
    | i <= 0 || i > (length l) = Nothing
    | otherwise = Just (l !! (i-1))

evaluateTree :: Tree -> Int
evaluateTree (Node [] xs) = sum xs
evaluateTree (Node ns xs) =
    sum $ map evaluateTree $ catMaybes (map (ix ns) xs)

main = do
    tree <- read <$> getContents
    print $ sumTree tree
    print $ evaluateTree tree
