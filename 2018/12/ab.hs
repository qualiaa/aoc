import Control.Monad (liftM2)
import Data.Either (rights)
import Data.List (elemIndex, find, isPrefixOf, notElem, tails, zip5)
import Data.Maybe (catMaybes)

type State = [Bool]
type Rule = (Bool,Bool,Bool,Bool,Bool)

charToCell c = case c of
                    '#' -> True
                    '.' -> False
                    otherwise -> error "Invalid character"

cellToChar c = if c then '#' else '.'

grow :: [Rule] -> State -> State
grow growRules state = pad 2 $ map (`elem` growRules) windows
    where [a,b,c,d,e] = take 5 $ tails state
          windows :: [Rule]
          windows = zip5 a b c d e

readRule :: String -> Either Rule Rule
readRule s = if grow then Right rule else Left rule
    where grow = charToCell . head $ drop 9 s 
          [a,b,c,d,e] = map charToCell $ take 5 s
          rule = (a,b,c,d,e)

pad n state = padding ++ state ++ padding
    where padding = replicate n False

isStable s s' = (fromStart s)  `isPrefixOf` (fromStart s') ||
                (fromStart s') `isPrefixOf` (fromStart s)
    where fromStart = dropWhile (not . id)

offset s s' = (-) <$> (elemIndex True s') <*> (elemIndex True s)

main = do
    let padN = 200

    initialState <- pad padN . map charToCell . dropWhile (`notElem` ".#") <$> getLine

    rules <- rights . map readRule . lines <$> (getLine >> getContents)

    let growthScheme = grow rules

        states = iterate growthScheme initialState

        targetState = states !! 20

        getPlantNumbers state = map fst $ filter snd numberedPots
            where numberedPots = zip [-padN..] state


    print . sum $ getPlantNumbers targetState


    let targetIteration = 50000000000

        statePairs = zip states (tail states)
    
        Just (iteration, stableStates) = find (uncurry isStable . snd) $ 
                                            zip [0..] statePairs

        velocity = uncurry offset stableStates
        time = targetIteration - iteration
        distance = fmap (time *) velocity

        plantNumbers = catMaybes . map (liftM2 (+) distance . return) $
                             getPlantNumbers (fst stableStates)
        
    print $ sum plantNumbers
