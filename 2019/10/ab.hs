import Data.Bifunctor (bimap)
import Data.Fixed (mod')
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.List (delete, uncons, sortOn, minimumBy, maximumBy, elemIndex)
import Data.Ratio ((%))
import qualified Data.Map.Strict as M

type Meteor = (Integer, Integer)

both f = bimap f f

disp :: Meteor -> Meteor -> (Integer, Integer)
disp (x,y) (x',y') =  (x' - x, y' - y)

dist :: Meteor -> Meteor -> Integer
dist a b = uncurry (+) . both abs $ disp a b

readMeteors :: [String] -> [Meteor]
readMeteors ls = [(i,j) | (j,l)   <- zip [0..] ls, (i,'#') <- zip [0..] l]

meteorDirections :: [Meteor] -> Meteor -> M.Map (Rational, Rational) [Meteor]
meteorDirections meteors m = M.fromListWith (++) (zip keys vals)
  where vals = map (:[]) $ delete m meteors 
        keys = map (dir . head) vals
        dir m' = disp m m' & both (% dist m m')

fireLaser :: (Fractional k, Ord k) => M.Map k [Meteor] -> [Meteor]
fireLaser meteors = fireLaser' meteors []
  where fireLaser' ms keys
          | M.null ms = []
          | null keys = fireLaser' ms $ M.keys ms
          | otherwise =
            let (k:ks)   = keys
                (m, ms') = removeMeteor ms k
            in m:fireLaser' ms' ks
        removeMeteor ms k = let (m:rest) = ms M.! k
                            in (m, if null rest then M.delete k ms
                                                else M.insert k rest ms)

main = do
  ms <- readMeteors . lines <$> getContents

  let meteorDirs = map (meteorDirections ms) ms
      visibleCounts = map M.size meteorDirs
      (count, monitoringStation) = maximumBy (comparing fst) $ zip visibleCounts ms
      
  print count 

  let angle x y = (atan2 y x + pi/2)  `mod'` (2*pi)
      (Just i) = elemIndex monitoringStation ms
      angleMap = meteorDirs !! i
                    & M.mapKeys (uncurry angle . both fromRational)
                    & M.map (sortOn (dist monitoringStation))
      (x,y) = fireLaser angleMap !! 199
      
  print $ 100*x + y
