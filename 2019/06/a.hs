import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Data.List (foldl')

type AList = [(String, String)]
type Counts = M.Map String Int

toAList :: [String] -> AList
toAList [] = []
toAList (line:rest) = (key,val):toAList rest
    where (val,_:key) = break (==')') line

-- Kind of ugly but more efficient than the naive approach
count :: AList -> Counts
count alist = foldl' builder M.empty alist
    where builder map (child, parent)
            | M.member child map  = map
            | M.member parent map = addTo map
            | isJust grandparent  = addTo $ builder map (parent, fromJust grandparent)
            | otherwise           = M.union map root

                where grandparent = lookup parent alist
                      addTo m = let val = fromJust $ M.lookup parent m in M.insert child (val+1) m
                      root = M.fromList [(child,1),(parent,0)]

main = print =<< M.foldl (+) 0 . count . toAList . lines <$> getContents



