{-# language RecordWildCards #-}

import Control.Applicative ((<|>))
import Data.Function (on)
import Data.List     (find, foldl', nub, sort, sortBy)
import Data.Maybe    (fromJust, fromMaybe, listToMaybe)
import Data.Monoid   ((<>))
import Data.Ord      (comparing, Down(Down))

import qualified Data.Map.Strict as M
import Army

type ID        = Int
type Groups    = M.Map ID Group
type TargetMap = M.Map ID ID

effectivePower :: Group -> Int
effectivePower g = (gDamage g) * (gUnits g)

attackOrder :: Group -> Group -> Ordering
attackOrder = comparing (Down . gInitiative)

targetSelectOrder :: Group -> Group -> Ordering
targetSelectOrder a b =
    (comparing (Down . effectivePower) a b) <>
    (comparing (Down . gInitiative)    a b)

targetPriority :: Group -> Group -> Group -> Ordering
targetPriority attacker a b =
    comparing (Down . (attacker `damageTo`)) a b <> targetSelectOrder a b

damageTo :: Group -> Group -> Int
damageTo attacker defender
    | targetIs "immune" = 0
    | targetIs "weak"   = effectivePower attacker * 2
    | otherwise         = effectivePower attacker

    where targetIs mod = fromMaybe False $
                            (attackType `elem`) <$> (gMods defender) M.!? mod
          attackType   = gDamageType attacker

selectTarget :: Groups -> Group -> Maybe ID
selectTarget groups attacker =
    let validTargets = M.toList $ M.filter isValid groups
    in fmap fst . listToMaybe $
            sortBy ((attacker `targetPriority`) `on` snd) validTargets
    where isValid target = gArmy attacker /= gArmy target &&
                            attacker `damageTo` target > 0

targetPhase :: Groups -> TargetMap
targetPhase groups =
    fst $ foldl' selectTarget' (M.empty, groups) orderedGroups

    where orderedGroups = sortBy (targetSelectOrder `on` snd) $ M.toList groups

          selectTarget' noChange @ (assignedTargets, possibleTargets)
                        (aID, attacker) =

            case selectTarget possibleTargets attacker of
                    Nothing  -> noChange
                    Just tID -> (M.insert aID tID assignedTargets,
                                 M.delete tID possibleTargets)

attack :: Group -> Group -> Maybe Group
attack attacker target =

    let damage      = attacker `damageTo` target
        unitsKilled = damage `div` gHp target
        newUnits    = gUnits target - unitsKilled

    in if newUnits > 0 then Just target { gUnits = newUnits }
                       else Nothing

attackPhase :: TargetMap -> Groups -> Groups

attackPhase targetMap groups = foldl' attackTarget groups orderedGroups

    where orderedGroups = map fst
                          $ sortBy (attackOrder `on` snd)
                          $ M.toList groups

          attackTarget groups' attackerID =

                let groups'' = do
                        attacker <- groups'   M.!? attackerID
                        targetID <- targetMap M.!? attackerID
                        return $ M.update (attacker `attack`) targetID groups'

                in fromJust $ groups'' <|> return groups'

fight :: Groups -> Groups
fight = targetPhase >>= attackPhase

finished :: Groups -> Bool
finished = (<=1) . length . nub . sort . map gArmy . M.elems

numUnits :: Groups -> Int
numUnits = sum . map gUnits . M.elems

isVictor :: Groups -> Bool
isVictor = (==1) . length . nub . sort . map gArmy . M.elems

reindeerWins :: Groups -> Bool
reindeerWins = (&&)
               <$> isVictor
               <*> ((=="Immune System") . gArmy . snd . head . M.toList)

giveBoost :: Int -> Groups -> Groups
giveBoost n = M.map (\g @ (Group {gArmy   = army,
                               gDamage = damage}) ->
                     if   army == "Immune System"
                     then g { gDamage=damage+n }
                     else g)

-- possibility of stalemate
finished' :: (Groups, Groups) -> Bool
finished' (lastState, currentState) =
    finished currentState || lastState == currentState


main = do
    armies <- map (getArmy . read . unlines) . splitOn "" . lines <$> getContents
    let groups = M.fromList . zip [0..] $ concat armies

        winningGroup = until finished fight groups

    print $ numUnits winningGroup
    
    let battle group = fromJust $ find finished' (zip states $ tail states)
            where states = iterate fight group

        boostedGroups = (giveBoost <$> [1..] <*> return groups)
        boostedResults  = map snd $ battle <$> boostedGroups

    print . numUnits  . fromJust $ find reindeerWins boostedResults


splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn sep l = case break (==sep) l of
    ([],  [])       -> []
    (init,[])       -> [init]
    ([],  sep:rest) -> splitOn sep rest
    (init,sep:rest) -> init:splitOn sep rest
