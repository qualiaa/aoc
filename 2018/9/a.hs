import Control.Monad (forM_)
--import qualified Data.Vector as V
import qualified Data.Map.Strict as M

nextMarble turn = (turn:) . rotl 2

--removeMarble 
--removeMarble = drop 1 . rotr 7

scoringTurn t = t `mod` 23 == 0


game numPlayers numMarbles = game' [0] $ M.fromList (zip [1..numPlayers] $ repeat 0)

    where game' state scores
            | turn > numMarbles = (state, scores)
            | scoringTurn turn  = scoringUpdate
            | otherwise         = normalUpdate
            
            where turn = head state + 1
    
                  normalUpdate  = let state' = nextMarble turn state
                                  in  game' state' scores
                  scoringUpdate =
                      let (marble:state') = rotr 7 state
                          player  = (turn - 1) `mod` numPlayers + 1
                          scoreMod= (marble + turn +)
                          state'' = nextMarble (turn + 1) state'
                          scores' = M.adjust scoreMod player scores
                      in game' state'' scores'


main = do
    pairs <- map ((\[a,b] -> (a,b)) . map read . words . filter (`elem` ' ':['0'..'9'])) . lines 
                <$> getContents :: IO [(Int,Int)]

    forM_ pairs (\(p,m) -> do
                    let res1 = game p m
                        res2 = game p (m*100)
                    print . maximum .  M.elems $ snd res1
                    print . maximum .  M.elems $ snd res2)
    {-
    forM pairs (\p -> do
        print 
    let fs = map (foldl1' (.) . flip replicate nextTurn) [1..25]
    mapM_ print $ zipWith ($) fs $ repeat []
    -}

rotl n = uncurry (flip (++)) . splitAt n
rotr n l = rotl (length l - n) l

{-
game numPlayers numMarbles = game' [0] $ M.fromList (zip [1..numPlayers] $ repeat 0)

    where game' state scores
            | turn > numMarbles = (state, scores)
            | scoringTurn turn  = scoringUpdate
            | otherwise         = normalUpdate
            
            where turn = head state + 1
    
                  normalUpdate  = let state' = nextMarble turn state
                                  in  game' state' scores
                  scoringUpdate =
                      let state'  = rotr 7 state
                          player  = (turn - 1) `mod` numPlayers + 1
                          scoreMod= head state' + turn
                          state'' = nextMarble (turn + 1) $ drop 1 state'
                          scores' = M.adjust (+scoreMod) player scores
                      in game' state'' scores'

-}
