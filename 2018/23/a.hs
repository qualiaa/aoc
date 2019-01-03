import Control.Monad (filterM)
import Data.Char (isDigit)
import Data.List (foldl', maximumBy)
import Data.Ord (comparing)
import Text.ParserCombinators.ReadP
import Text.Read(Read(readsPrec))


data Drone = Drone { pos :: (Int, Int, Int)
                   , r   :: Int
                   } deriving (Show, Eq)

instance Read Drone where
    readsPrec _ = readP_to_S readDrone

readDrone :: ReadP Drone
readDrone = do
    skipSpaces >> string "pos="
    pos <- ("("++) . (++")") <$> between (char '<') (char '>') (many get)
    string ", r="
    radius <- munch isDigit
    return $ Drone (read pos) (read radius)

man (x1,y1,z1) (x2,y2,z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

covers :: Drone -> Drone -> Bool
covers (Drone {pos=p1, r=r}) (Drone {pos=p2}) =  man p1 p2 <= r

coveredDrones :: Drone -> [Drone] -> [Drone]
coveredDrones d = filter (d `covers`)

-- XXX: This might not be correct
intersects :: Drone -> Drone -> Bool
intersects (Drone {pos=p1, r=r1})
           (Drone {pos=p2, r=r2}) = man p1 p2 <= (r1 + r2)

intersectsAll :: Drone -> [Drone] -> Bool
intersectsAll d = all (d `intersects`)

intersectedDrones :: Drone -> [Drone] -> [Drone]
intersectedDrones d = filter (d `intersects`)


intersectingGroups :: [Drone] -> [[Drone]]
intersectingGroups ds = do
    d <- ds
    return $ intersectGroup [d] delete d ds -- inefficient, could do fold (seen ++ x ++ rest)
    current

intersectGroup [d] = [[d]]
intersectGroup ds = do
    currentGroup, rest
    d <- ds
    guard $ d `intersectsAll`
    guard $ d `intersectsAll` current
    return $ intersectGroup (d:currentGroup) (delete d ds)
-}
    
    
    

{-
findCoveredDrones :: [Drone] -> Drone [Drone]
findCoveredDrones drones = foldl' coveringDrones [] drones

-}

findCoveredDrones :: [Drone] -> [[Drone]]
findCoveredDrones drones = foldl' coveringDrones [] drones

    where coveringDrones a x = filter (x `covers`) drones:a

main = do
    drones <- map read . lines <$> getContents
    print . length $ coveredDrones (maximumBy (comparing r) drones) drones

    print . length $ filterM (\_->[True,False]) drones
    
