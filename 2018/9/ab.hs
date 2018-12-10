import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
--import Data.Foldable (toList)
import qualified Deque as D
import qualified Data.Map.Strict as M

nextMarble turn = D.snoc turn . rotl 1
removeMarble state = let state' = rotr 7 state in (rotl 1 $ D.init state', dlast state')

scoringTurn t = t `mod` 23 == 0

rotr n = (!!n) . iterate D.shiftRight
rotl n = (!!n) . iterate D.shiftLeft

dlast = fromMaybe (error "tail on empty deque") . D.last

game numPlayers numMarbles = game' (D.fromList [0]) $ M.fromList (zip [1..numPlayers] $ repeat 0)

    where game' state scores
            | turn > numMarbles = (state, scores)
            | scoringTurn turn  = scoringUpdate
            | otherwise         = normalUpdate

            where turn = dlast state + 1

                  normalUpdate  = let state' = nextMarble turn state
                                  in  game' state' scores
                  scoringUpdate =
                      let (state',marble) = removeMarble state
                          player  = turn `mod` numPlayers
                          state'' = nextMarble (turn + 1) state'
                          scores' = M.adjust (marble + turn +) player scores
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
    let nextTurn :: D.Deque Int -> D.Deque Int
        nextTurn s
            | null s = D.fromList [0]
            | otherwise = let turn = dlast s + 1 in
                        if scoringTurn turn then nextMarble (turn+1) . fst $ removeMarble s
                                            else nextMarble turn s
    mapM_ (print . toList) (take 28 . iterate nextTurn $ D.fromList [])
    -}
