import Control.Monad (forM_, foldM, msum)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.Char (isDigit, isUpper)
import Data.List (foldl')
import Text.ParserCombinators.ReadP hiding (get)
import qualified Data.Map.Strict as M

type Substance = String
type Reagent = (Int, Substance)
type Reaction = (Int, [Reagent])
type ReactionTable = M.Map Substance Reaction
type Leftovers = M.Map Substance Int

type ReactionChain = ReaderT ReactionTable (State Leftovers)

-- Parsing
thing :: ReadP (Int, String)
thing = (,) <$> (read <$> munch1 isDigit) <*> (char ' ' >> munch1 isUpper)
reaction :: ReadP (Substance, Reaction)
reaction = do
  reagents <- sepBy1 thing (string ", ")
  (n, s) <- string " => " >> thing
  return (s, (n, reagents))

reactions :: ReadP ReactionTable
reactions = M.fromList <$> endBy1 reaction (char '\n') <* eof

readReactionTable :: String -> ReactionTable
readReactionTable = fst . head . readP_to_S reactions

-- Logic
fromLeftovers :: Substance -> Int -> ReactionChain Int
fromLeftovers sub amountRequested = do
  alreadyProduced <- M.findWithDefault 0 sub <$> get
  let diff = amountRequested - alreadyProduced
  -- Remove leftovers
  put . M.insert sub (max 0 (-diff)) =<< get
  -- Result is amount not satisfied by leftovers
  return $ max 0 diff

nLeftovers :: Substance -> ReactionChain Int
nLeftovers sub = do
  M.findWithDefault 0 sub <$> get

addLeftovers :: Int -> Substance -> ReactionChain ()
addLeftovers n substance = put . M.insertWith (+) substance n =<< get

getReaction :: Substance -> ReactionChain Reaction
getReaction s = asks (M.! s)

oreReq :: Int -> Substance -> ReactionChain Int
oreReq amountRequested "ORE" = return amountRequested
oreReq amountRequested substance = do

  amountNeeded <- fromLeftovers substance amountRequested

  if amountNeeded <= 0 then return 0 else do
    (amountProduced, reagents) <- getReaction substance

    let times = 1 + (amountNeeded - 1) `div` amountProduced

    addLeftovers (amountProduced*times - amountNeeded) substance
    foldM sumOre 0 $ map (first (*times)) reagents

  where sumOre ore reagent = (ore +) <$> uncurry oreReq reagent

bs f p min max
  | max - min <= 1 = if p (f max) then max else min
  | p (f centre) = bs f p centre max
  | otherwise    = bs f p min centre
  where centre = min + (max - min) `div` 2

runReaction table r = evalState (runReaderT r table) M.empty

main = do
  table <- readReactionTable <$> getContents
  let fuelOreReq = runReaction table . flip oreReq "FUEL"
      orePerOne = fuelOreReq 1
  print orePerOne
  let ore = 10^12
      min = ore `div` orePerOne
  print $ bs fuelOreReq (<ore) min (min*2)
