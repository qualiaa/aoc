{-# LANGUAGE LambdaCase #-}
import IntCode
import Control.Monad (forM_)
import Control.Monad.State.Lazy

type Score = Int
type Coord = (Int,Int)
data Tile = Empty | Wall | Block | Paddle | Ball deriving Enum

readObj :: [Int] -> Either (Tile, Coord) Score
readObj [-1,0,s] = Right s
readObj [x,y,t]  = Left (toEnum t, (x,y))

groupsOf _ [] = []
groupsOf n xs = let (ys,zs) = splitAt n xs in ys : groupN n zs

main = do
  program <- read . ('[':) . (++"]") <$> getContents
  let co = cocreate (2:tail program)

      (score,_,_) = flip execState (0,0,0) $
        withCoroutine co $ \output -> do
          let objs = map readObj $ groupsOf 3 output

          forM_ objs (\case
            Right s               -> modify (\(_,px,bx) -> (s,px,bx))
            Left (Paddle, (px,_)) -> modify (\(s, _,bx) -> (s,px,bx))
            Left (Ball,   (bx,_)) -> modify (\(s,px, _) -> (s,px,bx))
            _ -> return ())

          gets (\(_,px,bx) -> [signum $ bx - px])

  print score
