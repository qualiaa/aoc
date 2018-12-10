import qualified Data.Map as M
import Data.List (elemIndex, nub)
import Data.Functor ((<&>))
import Data.Maybe (maybeToList)

calcDistances coordinates x y = map distanceFunc coordinates
    where distanceFunc = \(a,b) -> abs (x - a) + abs (y - b)

calcDistanceTensor coordinates w h =
    [calcDistances coordinates x y | y <- [0..h-1], x <- [0..w-1]]

voronoi distanceTensor = do
    distances <- distanceTensor
    let minDistance = minimum distances
    if (length $ filter (==minDistance) distances) == 1
        then maybeToList $ elemIndex minDistance distances
        else return $ negate 1


takeEdges arr w h = edge [0  ..w-1]      ++ edge [ix 0 (h-1)   ..end] ++
                    edge [0,w..ix 0 h-1] ++ edge [w-1,ix (-1) 2..end]
    where ix i j = i + j * w
          edge = map (arr!!)
          end = w*h-1

normalise coordinates = (coordinates', w, h)
    where (xs, ys) = unzip coordinates
          l = minimum xs
          r = maximum xs
          t = minimum ys
          b = maximum ys
          w = r - l + 1
          h = b - t + 1
          coordinates' = zip (xs <&> (`minus` l)) (ys <&> (`minus` t))

main = do
    ordinates <- map read . words . filter (/=',') <$> getContents :: IO [Int]
    
    let coordinates' = dropEveryOther . zip ordinates $ tail ordinates
        (coordinates, w, h) = normalise coordinates'

        distanceTensor = calcDistanceTensor coordinates w h
        voronoiMap = voronoi distanceTensor

        edgeElements = nub $ takeEdges voronoiMap w h
        areas = M.fromList . zip [0..] $
                map (`countElem` voronoiMap) [0..length coordinates]
        maxArea = maximum . M.elems $ foldr M.delete areas edgeElements
        
    print maxArea
    print . length . filter (<10000) $ map sum distanceTensor

-- utils
dropEveryOther [] = []
dropEveryOther [x] = [x]
dropEveryOther (x:_:xs) = x:dropEveryOther xs
minus = (-)
countElem e = length . filter (==e)
