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
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Data.STRef
import qualified Data.Array.Unboxed as AU
import qualified Data.HashTable.ST.Basic as HT
import qualified Data.HashTable.Class as HTC

runProgram  :: ProgramSource -> Input -> (ProgramSource, Output)
evalProgram :: ProgramSource -> Input -> Output
execProgram :: ProgramSource -> Input -> ProgramSource

-- Interface types
type Value   = Int
type Address = Int
type ProgramSource = [Value]

data Termination = Success | Pause | Error deriving (Eq, Show)
data Coroutine = Paused (AU.UArray Address Value) Int Int Input
               | Finished Termination ProgramSource
               deriving Show


-- VM definition
type Memory s = (STUArray s Address Int)
type InstructionPtr   s = STRef s Address
type RelativeBase     s = STRef s Address
type ExtensibleMemory s = HT.HashTable s Address Value
type Environment      s = (Memory s, InstructionPtr s, RelativeBase s, ExtensibleMemory s)
type Input              = [Value]
type Output             = [Value]

type VM s = RWST (Environment s) Output Input (ST s)
type VMOp s = VM s (Maybe Termination)
data Instruction = Add | Mul | Get | Put | JIT | JIF | LT_ | Eql | RBO | End
                   deriving (Enum, Eq, Show)

-- Other Internal types
type FrozenMemory = AU.UArray Address Value
data Mode = Position | Immediate | Relative deriving (Enum, Eq)
data Parameter = P Mode Int

data VMOutput = VMOutput { vmMemory :: FrozenMemory
                         , vmIP :: Int
                         , vmRB :: Int
                         , vmUnconsumedInput :: Input
                         , vmOutput :: Output
                         , vmState :: Termination
                         }

-- Parsing
toMode :: Int -> Maybe Mode
toMode 0 = Just Position
toMode 1 = Just Immediate
toMode 2 = Just Relative
toMode _ = Nothing

toInstruction :: Int -> Maybe Instruction
toInstruction n
    | n > 0 && n < 10 = Just $ toEnum (n-1)
    | n == 99         = Just End
    | otherwise       = Nothing

interpretOpcode :: Value -> Maybe (Instruction, [Mode])
interpretOpcode n = (,) <$> toInstruction inst <*> (mapM toMode $ digits modes)
  where (modes, inst) = n `divMod` 100

-- VM state operations
-- Totally inefficient way of doing this
writeMem :: Address -> Value -> VM s ()
writeMem a v = do
  (mem,_,_,em) <- ask
  lift $ do
      (_,l) <- getBounds mem
      if a <= l then writeArray mem a v
                else HT.insert em a v

readMem :: Address -> VM s Value
readMem a = do
  (mem,_,_,em) <- ask
  (_,l) <- lift $ getBounds mem
  lift $ if a <= l then readArray mem a
                   else fromMaybe 0 <$> HT.lookup em a

moveIP :: Int -> VM s ()
setIP  :: Address -> VM s ()
getIP  :: VM s Address
moveRB :: Int -> VM s ()
getRB  :: VM s Address
moveIP n = do (_,ip,_,_) <- ask; lift $ modifySTRef ip (+ n)
setIP  n = do (_,ip,_,_) <- ask; lift $ writeSTRef ip n
getIP    = do (_,ip,_,_) <- ask; lift $ readSTRef ip
moveRB n = do (_,_,rb,_) <- ask; lift $ modifySTRef rb (+ n)
getRB    = do (_,_,rb,_) <- ask; lift $ readSTRef rb

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
executeInstruction RBO modes = rboOp modes
executeInstruction End _ = return Nothing

-- TODO: Catch invalid address error
binOp :: (Value -> Value -> Value) -> [Mode] -> VMOp s
binOp op modes = do
  [in1, in2, out] <- getParameters modes 3
  [x, y] <- mapM parameterToValue [in1, in2]
  addr <- getAddress out
  writeMem addr (op x y)
  return Nothing

-- TODO: Catch invalid address error
getOp :: [Mode] -> VMOp s
getOp modes =  do
  input <- getInput
  case input of
    Nothing -> return $ Just Pause
    Just input' -> do
      [addr] <- mapM getAddress =<< getParameters modes 1
      writeMem addr input' >> return Nothing

putOp :: [Mode] -> VMOp s
putOp modes = do
  [v] <- mapM parameterToValue =<< getParameters modes 1
  putOutput v >> return Nothing

jumpOp :: (Value -> Bool) -> [Mode] -> VMOp s
jumpOp p modes = do
  [test, target] <- mapM parameterToValue =<< getParameters modes 2
  when (p test) (setIP target) >> return Nothing

-- TODO: Catch invalid address error
cmpOp :: Ordering -> [Mode] -> VMOp s
cmpOp ord modes = do
  [in1, in2, out] <- getParameters modes 3
  addr <- getAddress out
  [x, y] <- mapM parameterToValue [in1, in2]
  writeMem addr $ if compare x y == ord then 1 else 0
  return Nothing

rboOp :: [Mode] -> VMOp s
rboOp modes = do
  [v] <- mapM parameterToValue =<< getParameters modes 1
  moveRB v >> return Nothing

-- Execution
getInstruction :: VM s (Maybe (Instruction, [Mode]))
getInstruction = interpretOpcode <$> (readMem =<< getIP)

parameterToValue :: Parameter -> VM s Value
parameterToValue (P Immediate v) = return v
parameterToValue p  = getAddress p >>= readMem
getAddress (P Position a)  = return a
getAddress (P Immediate _) = fail "oh fuck"
getAddress (P Relative r)  = (r+) <$> getRB

getParameters :: [Mode] -> Int -> VM s [Parameter]
getParameters modes n = do
  ip <- getIP
  vals <- mapM readMem $ take n [ip+1..]
  let modes' = ZipList $ modes ++ repeat Position
  moveIP $ n + 1
  return . getZipList $ P <$> modes' <*> ZipList vals


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

startVM :: FrozenMemory -> Int -> Int -> Input -> VMOutput
startVM mem ip rb input = runST $ do
      env@(mem',ip',rb',em') <- (,,,) <$> unsafeThaw mem
                                      <*> newSTRef ip
                                      <*> newSTRef rb
                                      <*> HT.new
      (term, unconsumedInput, output) <- runRWST nextInstruction env input

      -- Return memory appended with extended memory
      l <- snd <$> getBounds mem'
      usedEM <- HTC.toList em'
      let u = maximum $ map fst usedEM
          m = if null usedEM
                then unsafeFreezeSTUArray mem'
                else AU.accumArray (\_ a -> a) 0 (0, u) <$>
                              ((++) <$> getAssocs mem' <*> return ((zip [l+1..u] $ repeat 0) ++ usedEM))

      VMOutput <$> m
               <*> readSTRef ip'
               <*> readSTRef rb'
               <*> pure unconsumedInput
               <*> pure output
               <*> pure term
-- The compile errors for point free style are _not_ nice here >_>

-- Entrypoints
runProgram  p input = (AU.elems $ vmMemory vm, vmOutput vm)
  where vm = startVM (AU.listArray (0,length p - 1) p) 0 0 input
evalProgram p = snd . runProgram p
execProgram p = fst . runProgram p

outputToCoroutine :: VMOutput -> (Coroutine, Output)
outputToCoroutine vm = (co vm, vmOutput vm)
  where co VMOutput{vmState = Pause} = Paused (vmMemory vm)
                                              (vmIP vm)
                                              (vmRB vm)
                                              (vmUnconsumedInput vm)
        co _ = Finished (vmState vm) (AU.elems $ vmMemory vm)

coroutine :: ProgramSource -> Input -> (Coroutine, Output)
coroutine p input = resume (cocreate p) input

cocreate :: ProgramSource -> Coroutine
cocreate p = Paused (AU.listArray (0,length p - 1) p) 0 0 []

resume :: Coroutine -> Input -> (Coroutine, Output)
resume co@(Finished _ _) _ = (co, [])
resume (Paused memory ip rb oldInput) newInput =
  outputToCoroutine $ startVM memory ip rb (oldInput ++ newInput)

-- Convenience functions
digits 0 = []
digits n = let (q,r) = n `divMod` 10 in r:digits q
