import qualified Data.Vector as V
import Data.List ((\\), nub)
import Data.Maybe (maybeToList)

import Patch

width  = 1000
height = 1000

intersection :: Rect -> Rect -> Maybe Rect
intersection (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
    if w > 0 && h > 0 then Just (Rect l t w h) else Nothing
    where l = max x1 x2 
          r = min (x1 + w1) (x2 + w2)
          t = max y1 y2
          b = min (y1 + h1) (y2 + h2)
          w = r - l
          h = b - t

intersectAll :: [(Int, Rect)] -> [((Int, Int), Rect)]
intersectAll l = do
    (i, a) <- l
    (j, b) <- drop i l
    x <- maybeToList $ intersection a b
    return ((i, j), x)

coveredIndices :: Rect -> [Int]
coveredIndices (Rect x y w h) = [i+j*width | i <- [x..x+w-1], j <- [y..y+h-1]]

type Bitmap = V.Vector Bool
unionAsMask :: [Rect] -> [Bool]
unionAsMask rects = V.toList $ foldr addRectToBitmap base rects
    where addRectToBitmap :: Rect -> Bitmap -> Bitmap
          addRectToBitmap rect = flip V.unsafeUpd $ zip (coveredIndices rect) (repeat True) 

          base :: Bitmap
          base = V.replicate (width*height) False

main = do
    patches <- map (getPatch . read) . lines <$> getContents
    let intersections = intersectAll patches

    -- part A
    print . length . filter id . unionAsMask $ map snd intersections

    -- part B
    let ids      = map fst patches
        (is, js) = unzip $ map fst intersections
    
    print $ ids \\ (nub (is ++ js))

    
