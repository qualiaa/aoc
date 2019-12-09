import Control.Monad (forM_)
import Data.Char(isDigit)
import Data.Function(on)
import Data.List(minimumBy, group, sort, foldl1')

w = 25
h = 6
size = w * h

takeEvery :: Int -> [a] -> [[a]]
takeEvery _ [] = []
takeEvery n l = let (front, back) = splitAt n l in front : takeEvery n back

toLayers :: String -> [[Int]]
toLayers s = map (map (read . (:[]))) $ takeEvery size s

decode :: [[Int]] -> [Int]
decode ls = foldl1' (\i l -> map set $ zip i l) ls
  where set (i,l) = if i == 2 then l else i

main = do
  layers <- toLayers . filter isDigit <$> getContents
  let count n = length . filter (==n)
      best = minimumBy (compare `on` (count 0)) layers
  print $ (count 1 best) * (count 2 best)

  forM_ (takeEvery w $ decode layers) $
    putStrLn . map (\x -> if x == 0 then ' ' else '#')
