{-# Language FlexibleInstances #-}
module IntCode
( runProgram
, evalProgram
, execProgram
, Input
, Output
, Program
) where

import Control.Applicative (ZipList(..))
import Control.Arrow (first)
import Control.Monad (when)
import Control.Monad.RWS.Strict
import Control.Monad.ST
import Data.Array.Base (unsafeFreezeSTUArray)
import Data.Array.ST
import Data.Either (isLeft, fromLeft)
import Data.Maybe (fromJust)
import Data.List (uncons)
import Data.STRef
import qualified Data.Array.Unboxed as AU

runProgram  :: Program -> Input -> (Program, Output)
evalProgram :: Program -> Input -> Output
execProgram :: Program -> Input -> Program

instance Show (Int -> Parameter) where
  show m = if isLeft (m 0) then "LEFT!" else "RIGHT!"

data Instruction = Add | Mul | Get | Put | JIT | JIF | LT_ | Eql | End deriving (Eq, Show)
type Value   = Int
type Address = Int
type Parameter = Either Address Value
type Mode = Int -> Parameter

type Program = [Value]

-- VM definition
type Memory s = (STUArray s Address Int)
type InstructionPtr s = STRef s Address
type Environment s = (Memory s, InstructionPtr s)
type Input         = [Value]
type Output        = [Value]
type VM s = RWST (Environment s) Output Input (ST s)

-- Parsing
toModes :: [Int] -> Maybe [Mode]
toModes = sequence . map toMode
    where toMode 0 = Just Left
          toMode 1 = Just Right
          toMode _ = Nothing


interpretOpcode :: Value -> Maybe (Instruction, [Mode])
interpretOpcode n = (,) <$> toInstruction inst <*> (toModes $ digits modes)
    where (modes, inst) = n `divMod` 100

parameterToValue :: Parameter -> VM s Value
parameterToValue (Right p) = return p
parameterToValue (Left p) = readMem p

-- VM state operations
writeMem :: Address -> Value -> VM s ()
writeMem a v = do
  text <- fst <$> ask
  lift $ writeArray text a v
readMem :: Address -> VM s Value
readMem a = do
  text <- fst <$> ask
  lift $ readArray text a

moveIP :: Int -> VM s ()
setIP  :: Address -> VM s ()
getIP  :: VM s Address
moveIP n = do ip <- snd <$> ask; lift $ modifySTRef ip (+ n)
setIP  n = do ip <- snd <$> ask; lift $ writeSTRef ip n
getIP    = do ip <- snd <$> ask; lift $ readSTRef ip

getInput :: VM s Value
getInput = state (fromJust . uncons)

putOutput :: Value -> VM s ()
putOutput v = tell [v]

-- Operations
toInstruction :: Int -> Maybe Instruction
toInstruction 1  = Just Add
toInstruction 2  = Just Mul
toInstruction 3  = Just Get
toInstruction 4  = Just Put
toInstruction 5  = Just JIT
toInstruction 6  = Just JIF
toInstruction 7  = Just LT_
toInstruction 8  = Just Eql
toInstruction 99 = Just End
toInstruction _  = Nothing

executeInstruction Add modes = binOp (+) modes
executeInstruction Mul modes = binOp (*) modes
executeInstruction Get modes = getOp modes
executeInstruction Put modes = putOp modes
executeInstruction JIT modes = jumpOp (/=0) modes
executeInstruction JIF modes = jumpOp (==0) modes
executeInstruction LT_ modes = cmpOp LT modes
executeInstruction Eql modes = cmpOp EQ modes
executeInstruction End _ = return ()

binOp :: (Value -> Value -> Value) -> [Mode] -> VM s ()
binOp op modes = do
  [in1, in2, out] <- getParameters modes 3
  [x, y] <- mapM parameterToValue [in1, in2]
  writeMem (getAddress out) (op x y)

getOp :: [Mode] -> VM s ()
getOp modes =  do
  [addr] <- fmap getAddress <$> getParameters modes 1
  getInput >>= writeMem addr

putOp :: [Mode] -> VM s ()
putOp modes = do
  [v] <- mapM parameterToValue =<< getParameters modes 1
  putOutput v

jumpOp :: (Value -> Bool) -> [Mode] -> VM s ()
jumpOp p modes = do
    [test, addr] <- mapM parameterToValue =<< getParameters modes 2
    when (p test) $ setIP addr

cmpOp :: Ordering -> [Mode] -> VM s ()
cmpOp ord modes = do
    [in1, in2, out] <- getParameters modes 3
    [x, y] <- mapM parameterToValue [in1, in2]
    writeMem (getAddress out) $ if compare x y == ord then 1 else 0


-- Execution
getInstruction :: VM s (Maybe (Instruction, [Mode]))
getInstruction = interpretOpcode <$> (readMem =<< getIP)

getParameters :: [Mode] -> Int -> VM s [Parameter]
getParameters modes n = do
  ip <- getIP
  vals <- mapM readMem $ take n [ip+1..]
  let modes' = ZipList $ modes ++ repeat Left
  moveIP $ n + 1
  return $ getZipList $ modes' <*> ZipList vals


nextInstruction :: VM s ()
nextInstruction = do
  inst <- getInstruction
  case inst of
    Nothing             -> writeMem 0 (-1) -- clear first register on error
    Just (End,_)        -> return ()
    Just (inst', modes) -> do
        executeInstruction inst' modes
        nextInstruction

-- Entrypoints
runProgram p input = first AU.elems $ runST (do
      text <- newListArray (0, length p - 1) p
      ip <- newSTRef 0
      (_, w) <- execRWST nextInstruction (text,ip) input
      (,) <$> unsafeFreezeSTUArray text <*> return w
  )
-- The compile errors for point free style are _not_ nice here >_>

evalProgram p = snd . runProgram p
execProgram p = fst . runProgram p

-- Convenience functions
digits 0 = []
digits n = let (q,r) = n `divMod` 10 in r:digits q
getAddress = either id (fail "oh fuck")
