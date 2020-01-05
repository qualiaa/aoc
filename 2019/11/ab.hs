import Prelude hiding (Either(..))
import Control.Monad (forM_)
import Data.Bifunctor (first,second)
import Data.Ix (range)
import IntCode
import qualified Data.Map.Strict as M

type Coord = (Int,Int)
data Paint = Black | White deriving (Eq, Enum, Show)
data Dir = Up | Right | Down | Left deriving (Enum, Bounded, Eq, Show)

next :: (Eq a, Enum a, Bounded a) => a -> a
next v = if v == maxBound then minBound else succ v
prev :: (Eq a, Enum a, Bounded a) => a -> a
prev v = if v == minBound then maxBound else pred v

move Right = first succ
move Left  = first pred
move Down  = second succ
move Up    = second pred

runRobot :: Coroutine -> M.Map Coord [Paint] -> M.Map Coord [Paint]
runRobot robot panels = paintSquare robot (0,0) Up panels
  where paintSquare (Finished _ _) _ _ m = m
        paintSquare co loc dir m =
          let col = head $ M.findWithDefault [Black] loc m
              (co',[c, r]) = resume co [fromEnum col]
              col' = toEnum c
              dir' = if r == 0 then prev dir else next dir
              loc' = move dir' loc
              m' = if col /= col' then M.insertWith (++) loc [col'] m else m
          in paintSquare co' loc' dir' m'

minmax l = (minimum l, maximum l)

main = do
  program <- read . ('[':) . (++"]") <$> getContents
  let robot = cocreate program
      paintMap = runRobot robot M.empty
  print $ M.size paintMap

  let paintMap' = runRobot robot $ M.singleton (0,0) [White]
      whitePanels = M.filter (==White) $ M.map head paintMap'
      panelCoords = M.keys whitePanels
      (xBounds,yBounds) = (minmax (map fst panelCoords),
                           minmax (map snd panelCoords))

  forM_ (range yBounds) $ (\y -> do
    forM_ (range xBounds) $ (\x -> do
      putChar $ if M.member (x,y) whitePanels then '#' else ' ')
    putChar '\n')
