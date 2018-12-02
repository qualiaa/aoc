readInt :: String -> Int
readInt ('+':s) = read s
readInt s = read s

-- obviously doesn't work for finite lists
findRepeat l = findRepeat' l []

    where findRepeat' :: (Eq a) => [a] -> [a] -> a
          findRepeat' (x:xs) [] = if x `elem` ys then x else findRepeat' (x : ys) xs

main = do
    dfs <- cycle . map readInt . lines <$> getContents
    let fs = scanl (+) 0 df
    print $ findRepeat fs
