import Data.Char(isUpper,isLower,toLower)
import Data.List(delete)

singlePass "" = ""
singlePass [c] = [c]
singlePass (a:b:cs) = if match a b then singlePass cs else a:singlePass (b:cs)
    where match a b = a `aA` b || b `aA` a
          aA a b = isLower a && isUpper b && a == toLower b

reduce s
    | s' == s = s
    | otherwise = reduce s'
    where s' = singlePass s

sift :: String -> [String]
sift s = do
    element <- zipWith (\a b -> a:b:[]) ['a'..'z'] ['A'..'Z']
    return $ filter (`notElem` element) s

main = do
    input <- delete '\n' <$> getContents
    print . length $ reduce input
    print . minimum . map (length . reduce) $ sift input
