import Data.List(maximumBy, foldl1')
import Data.Function(on)

rotSum :: Int -> [Int] -> [Int]
rotSum n l
    | n < 2 = l
    | otherwise = rotSum' (sum front) (zip back l)

        where (front, back) = splitAt (n-1) l

              rotSum' _ [] = []
              rotSum' s ((x,v):xvs) = s':rotSum' (s'-v) xvs
                    where s' = x+s

rotSum2D :: Int -> [[Int]] -> [[Int]]
rotSum2D n l
    | n < 2 = l
    | otherwise = rotSum2D' (listSum front) (zip back l')

        where (front, back) = splitAt (n-1) l'
              (+:+)         = zipWith (+)
              (-:-)         = zipWith (-)
              listSum       = foldl1' (+:+)
              l'            = map (rotSum n) l

              rotSum2D' _ [] = []
              rotSum2D' s ((x,v):xvs) = s': rotSum2D' (s' -:- v) xvs
                    where s' = x +:+ s

main = do
    s <- read <$> getContents :: IO Int

    let p x y = (y*r+s)*r `div` 100 `mod` 10 - 5
            where r = x+10

        grid = [[p x y | x <- [1..300]] | y <- [1..300]]

    let maxIndex = snd . maxAndArg . concat $ rotSum2D 3 grid
        indexFromK = indexTo2D . (301-)

    print $ indexFromK 3 maxIndex

    let maxSums = map (maxAndArg . concat . flip rotSum2D grid) [1..300]

        ((_,maxIndex'),k) = maximumBy (compare `on` fst . fst) (zip maxSums [1..])

        (x, y) = indexFromK k maxIndex'
        result = (x,y,k)
        
    print result


indexTo2D w i = (i `mod` w + 1, i `div` w + 1)

maxAndArg l = maximumBy (compare `on` fst) (zip l [0..])
