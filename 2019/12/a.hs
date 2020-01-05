import Control.Applicative(ZipList(..),liftA2)
import Control.Monad(forM_)
import Data.Char(isDigit)
import Data.List(elemIndex,foldl')

type Vector = ZipList Int

cmp x y = fromEnum (compare x y) - 1
add = liftA2 (+)
findGravity = liftA2 cmp

applyGravity :: [(Vector,Vector)] -> [Vector]
applyGravity pvs = do
  (p,v) <- pvs
  let ps = map fst pvs
      as = map (flip findGravity p) ps
  return $! foldl' add v as

step :: ([Vector], [Vector]) -> ([Vector], [Vector])
step (p,v) = (p', v')
  where v' = applyGravity $ zip p v
        p' = zipWith add p v'

energy p v = sum $ zipWith (*) (e p) (e v)
  where e = map (sum . fmap abs)

readVector :: String -> Vector
readVector = ZipList . read . ('[':) . (++"]") . filter (\x -> x `elem` ",\n-" || isDigit x)

main = do
  ps <- map readVector . lines  <$> getContents
  let vs = replicate 4 $ ZipList [0,0,0]
      init = (ps,vs)

      simulation = iterate step init

  print . uncurry energy $ simulation !! 1000

  -- massive space leak uncovered by this line
  print . elemIndex (ps,vs) $ tail simulation
