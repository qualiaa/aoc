import Control.Applicative ((<|>))
import Data.Maybe (isJust, fromJust)
import Data.List (elemIndex)

type Body = String
type AList = [(Body, Body)]
type Path = [Body]

toAList :: [Body] -> AList
toAList [] = []
toAList (line:rest) = (key,val):toAList rest
    where (val,_:key) = break (==')') line

pathToAncestor :: AList -> Body -> Body -> Maybe Path
pathToAncestor alist ancestor child =
  if ancestor == child then Just [ancestor]
        else (child:) <$> (lookup child alist >>= pathToAncestor alist ancestor)

findDivergence [] _ = Nothing
findDivergence _ [] = Nothing
findDivergence (a:as) (b:bs)
    | a == b = findDivergence as bs <|> Just a
    | a /= b = Nothing

main = do
  alist <- toAList . lines <$> getContents
  let path = pathToAncestor alist "COM"
      Just (d1, d2) = do
        santaPath <- path "SAN"
        myPath    <- path "YOU"
        divergence <- findDivergence (reverse santaPath) (reverse myPath)
        (,) <$> elemIndex divergence santaPath
            <*> elemIndex divergence myPath

  print $ d1 + d2 - 2
