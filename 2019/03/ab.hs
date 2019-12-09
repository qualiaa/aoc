import Control.Arrow(first, second)
import Data.Function(on)
import Data.List(foldl', minimumBy)
import Data.Ix(range)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Coord = (Int, Int)
type Move = Coord -> [Coord]

performMoves :: [Move] -> [Coord]
performMoves = foldl' (\(pos:hist) op -> op pos ++ hist) [(0,0)]

toMoves :: String -> [Move]
toMoves =  map toMove . splitOn ','
  where toMove (x:num) = toMove' x (read num :: Int)
        toMove' 'R' = move first  (+)
        toMove' 'L' = move first  (flip (-))
        toMove' 'U' = move second (+)
        toMove' 'D' = move second (flip (-))
        move pos dir n c = map pos (dir <$> [n,n-1..0]) <*> pure c

main = do
  [w1,w2] <- map (init . performMoves . toMoves) . lines <$> getContents
  let intersections = S.intersection (S.fromList w1) (S.fromList w2)
  print . S.findMin $ S.map (\(x,y) -> abs x + abs y) intersections

  let c1 = M.fromList (zip (reverse w1) [1..])
      c2 = M.fromList (zip (reverse w2) [1..])

      distances = M.unionWith (+)
        (M.restrictKeys c1 intersections)
        (M.restrictKeys c2 intersections)

  print . minimum $ M.elems distances

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn sep l = case break (==sep) l of
    ([],  [])       -> []
    (init,[])       -> [init]
    ([],  sep:rest) -> splitOn sep rest
    (init,sep:rest) -> init:splitOn sep rest
