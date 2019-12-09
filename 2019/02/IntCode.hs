module IntCode
( runProgram
, evalProgram
, execProgram
, coroutine
, resume
, Coroutine(..)
, Termination(..)
, Input
, Output
, ProgramSource
) where

import Control.Applicative (ZipList(..))
import Control.Arrow (first)
import Control.Monad (when)
import Control.Monad.RWS.Strict
import Control.Monad.ST
import Data.Array.Base (unsafeFreezeSTUArray, unsafeThaw)
import Data.Array.ST
import Data.Either (isLeft, fromLeft)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (uncons)
import Data.STRef
import qualified Data.Array.Unboxed as AU

runProgram  :: ProgramSource -> Input -> (ProgramSource, Output)
evalProgram :: ProgramSource -> Input -> Output
execProgram :: ProgramSource -> Input -> ProgramSource

-- Interface types
type Value   = Int
type Address = Int
type ProgramSource = [Value]

data Termination = Success | Pause | Error deriving (Eq, Show)
data Coroutine = Paused (AU.UArray Address Value) Int Input
               | Finished Termination ProgramSource


-- VM definition
type Memory s = (STUArray s Address Int)
type InstructionPtr s = STRef s Address
type Environment s = (Memory s, InstructionPtr s)
type Input         = [Value]
type Output        = [Value]
type VM s = RWST (Environment s) Output Input (ST s)
type VMOp s = VM s (Maybe Termination)
data Instruction = Add | Mul | Get | Put | JIT | JIF | LT_ | Eql | End
                   deriving (Eq, Show)

-- Other Internal types
type FrozenMemory = AU.UArray Address Value
type Parameter = Either Address Value
type Mode = Int -> Parameter

-- Parsing
toModes :: [Int] -> Maybe [Mode]
toModes = sequence . map toMode
  where toMode 0 = Just Left
        toMode 1 = Just Right
        toMode _ = Nothing

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

interpretOpcode :: Value -> Maybe (Instruction, [Mode])
interpretOpcode n = (,) <$> toInstruction inst <*> (toModes $ digits modes)
  where (modes, inst) = n `divMod` 100

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

getInput :: VM s (Maybe Value)
getInput = state (maybe (Nothing, []) (first Just) . uncons)

putOutput :: Value -> VM s ()
putOutput v = tell [v]

-- Operations
executeInstruction Add modes = binOp (+) modes
executeInstruction Mul modes = binOp (*) modes
executeInstruction Get modes = getOp modes
executeInstruction Put modes = putOp modes
executeInstruction JIT modes = jumpOp (/=0) modes
executeInstruction JIF modes = jumpOp (==0) modes
executeInstruction LT_ modes = cmpOp LT modes
executeInstruction Eql modes = cmpOp EQ modes
executeInstruction End _ = return Nothing

-- TODO: Catch invalid address error
binOp :: (Value -> Value -> Value) -> [Mode] -> VMOp s
binOp op modes = do
  [in1, in2, out] <- getParameters modes 3
  [x, y] <- mapM parameterToValue [in1, in2]
  writeMem (getAddress out) (op x y)
  return Nothing

-- TODO: Catch invalid address error
getOp :: [Mode] -> VMOp s
getOp modes =  do
  input <- getInput
  case input of
    Nothing -> return $ Just Pause
    Just input' -> do
      [addr] <- fmap getAddress <$> getParameters modes 1
      writeMem addr input'
      return Nothing

putOp :: [Mode] -> VMOp s
putOp modes = do
  [v] <- mapM parameterToValue =<< getParameters modes 1
  putOutput v
  return Nothing

jumpOp :: (Value -> Bool) -> [Mode] -> VMOp s
jumpOp p modes = do
  [test, addr] <- mapM parameterToValue =<< getParameters modes 2
  when (p test) $ setIP addr
  return Nothing

-- TODO: Catch invalid address error
cmpOp :: Ordering -> [Mode] -> VMOp s
cmpOp ord modes = do
  [in1, in2, out] <- getParameters modes 3
  [x, y] <- mapM parameterToValue [in1, in2]
  writeMem (getAddress out) $ if compare x y == ord then 1 else 0
  return Nothing


-- Execution
getInstruction :: VM s (Maybe (Instruction, [Mode]))
getInstruction = interpretOpcode <$> (readMem =<< getIP)

parameterToValue :: Parameter -> VM s Value
parameterToValue (Right p) = return p
parameterToValue (Left p) = readMem p

getParameters :: [Mode] -> Int -> VM s [Parameter]
getParameters modes n = do
  ip <- getIP
  vals <- mapM readMem $ take n [ip+1..]
  let modes' = ZipList $ modes ++ repeat Left
  moveIP $ n + 1
  return $ getZipList $ modes' <*> ZipList vals


nextInstruction :: VM s Termination
nextInstruction = do
  inst <- getInstruction
  case inst of
    Nothing             -> return Error
    Just (End,_)        -> return Success
    Just (inst', modes) -> do
      termination <- executeInstruction inst' modes
      case termination of
        Nothing -> nextInstruction
        Just t -> return t

startVM :: FrozenMemory -> Int -> Input -> VMOutput
startVM frozenMemory ptr input = runST $ do
      memory <- unsafeThaw frozenMemory
      ip <- newSTRef ptr
      (term, unconsumedInput, output) <- runRWST nextInstruction (memory, ip) input

      VMOutput <$> unsafeFreezeSTUArray memory
               <*> readSTRef ip
               <*> pure unconsumedInput
               <*> pure output
               <*> pure term
-- The compile errors for point free style are _not_ nice here >_>

-- Entrypoints
data VMOutput = VMOutput { vmMemory :: FrozenMemory
                         , vmIP :: Int
                         , vmUnconsumedInput :: Input
                         , vmOutput :: Output
                         , vmState :: Termination
                         }

runProgram  p input = (AU.elems $ vmMemory vm, vmOutput vm)
  where vm = startVM (AU.listArray (0,length p - 1) p) 0 input
evalProgram p = snd . runProgram p
execProgram p = fst . runProgram p

outputToCoroutine :: VMOutput -> (Coroutine, Output)
outputToCoroutine vm = (co vm, vmOutput vm)
  where co VMOutput{vmState = Pause} = Paused (vmMemory vm) (vmIP vm) (vmUnconsumedInput vm)
        co _ = Finished (vmState vm) (AU.elems $ vmMemory vm)

coroutine :: ProgramSource -> Input -> (Coroutine, Output)
coroutine p input = resume (cocreate p) input

cocreate :: ProgramSource -> Coroutine
cocreate p = Paused (AU.listArray (0,length p - 1) p) 0 []

resume :: Coroutine -> Input -> (Coroutine, Output)
resume co@(Finished _ _) _ = (co, [])
resume (Paused memory ip oldInput) newInput =
  outputToCoroutine $ startVM memory ip (oldInput ++ newInput)

-- Convenience functions
digits 0 = []
digits n = let (q,r) = n `divMod` 10 in r:digits q
getAddress = either id (fail "oh fuck")
