import Data.List (group, nub, sort)

numAppearing x = length . filter (x `elem`)

main = do
    counts <- map (map length . group . sort) . lines <$> getContents
    print $ numAppearing 2 counts * numAppearing 3 counts

{- eqivalent to a.sh 
eq2or3 x = x == 2 || x == 3

main = product . map length . group . sort . concat . map (nub . filter eq2or3 . map length . group . sort) . lines <$> getContents >>= print
    where eq2or3 x = x == 2 || x == 3
-}

