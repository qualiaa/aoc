import Data.Char

addToPolymer [] u = [u]
addToPolymer (u0:p) u1
    | abs (u0-u1) == 32 = p
    | otherwise = u1:u0:p
newPolymer = foldl addToPolymer []

main = do
    input <- map ord . filter (/='\n') <$> getContents
    
    print . length $ newPolymer input
    let siftedInputs = map (\i -> filter (`notElem` [i,i+32]) $ input) [ord 'A'..ord 'Z']
    print . minimum $ length . newPolymer <$> siftedInputs
