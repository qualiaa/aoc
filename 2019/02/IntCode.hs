module IntCode
( runProgram
, Program
) where

import Control.Monad (unless)
import Control.Monad.Reader
import Control.Monad.ST
import Data.Array.ST
import Data.Maybe
import Data.STRef
import qualified Data.Array.Unboxed as AU

data Instruction = Add | Mul | End deriving Eq
type Value = Int
type Address = Int
type Program = [Value]
type Array s = (STUArray s Address Value)
type InstructionPtr s = STRef s Address

type VMState s = (Array s, InstructionPtr s)
type VMOperation s a = ReaderT (VMState s) (ST s) a

toInstruction :: Value -> Maybe Instruction
toInstruction 1  = Just Add
toInstruction 2  = Just Mul
toInstruction 99 = Just End
toInstruction _ = Nothing

executeInstruction :: Instruction -> VMOperation s ()
executeInstruction Add = binOp (+)
executeInstruction Mul = binOp (*)
executeInstruction End = return ()

binOp :: (Value -> Value -> Value) -> VMOperation s ()
binOp op = do
  text <- fst <$> ask
  [in1, in2, out] <- getArgs 3
  [x, y] <- lift $ mapM (readArray text) [in1, in2]
  lift $ writeArray text out (op x y)
  moveIP 4

moveIP :: Int -> VMOperation s ()
moveIP n = do
  ip <- snd <$> ask
  lift $ modifySTRef ip (+ n)

setIP :: Int -> VMOperation s ()
setIP n = do
  ip <- snd <$> ask
  lift $ writeSTRef ip n

getInstruction :: VMOperation s (Maybe Instruction)
getInstruction = do
  (text, ip) <- ask
  toInstruction <$> lift ((readArray text =<< readSTRef ip))

getArgs :: Int -> VMOperation s [Address]
getArgs n = do
  (text, ip) <- ask
  ip_v <- (lift . readSTRef) ip
  lift $ mapM (readArray text ) $ take n [ip_v+1..]

nextInstruction :: VMOperation s ()
nextInstruction = do
  inst <- getInstruction
  text <- fst <$> ask
  case inst of
    Nothing    -> lift $ writeArray text 0 (-1) -- clear first register on error
    Just End   -> return ()
    Just inst' -> executeInstruction inst' >> nextInstruction

runProgram' :: Program -> ST s (Array s)
runProgram' p = do
  text <- newListArray (0, length p - 1) p
  ip <- newSTRef 0
  runReaderT nextInstruction (text,ip)
  return text

runProgram :: Program -> Program
runProgram p = AU.elems $ runSTUArray $ runProgram' p
-- The compile errors for point free style are _not_ nice here >_>
