{-# LANGUAGE RecordWildCards #-}

import Instruction
import Data.List (foldl')
import Data.Either (rights)
import qualified Data.Map.Strict as M

type Binds = M.Map Int IPBind
type Insts = M.Map Int Instruction

labelAddresses :: Program -> M.Map Int Instruction
labelAddresses = M.fromList . zip [0..] . rights

labelBinds :: Program -> M.Map Int IPBind
labelBinds = fst . foldl' label (M.empty, 0)
    where label (m,i) (Left ip) = (M.insert i ip m, i)
          label (m,i) (Right _) = (m, succ i)

runInstruction :: Instruction -> Registers -> Registers
runInstruction (op, a, b, c) = runOp op (a,b,c)

-- this approach is more general than required for the question, #ip specs can
-- appear anywhere in the program
runProgram :: Binds -> Insts -> Registers
runProgram binds insts = runFromLine 0 $ replicate 6 0


    where runFromLine :: Int -> Registers -> Registers
          runFromLine line reg =
                case (insts M.!? line) of
                    Nothing -> reg
                    Just inst' ->
                        let reg' = runInstruction inst' reg
                            (line', reg'') = findNextLine binds line reg'
                        in runFromLine line' reg''

findNextLine :: Binds -> Int -> Registers -> (Int, Registers)
findNextLine binds line reg = maybe (line+1, reg) updateIP (snd <$> M.lookupLE line binds)
    where updateIP ipBind = let nextLine = (reg !! ipBind) + 1
                            in (nextLine, modReg reg ipBind nextLine)

main = do
    program <- parseInput <$> getContents
    let insts = labelAddresses program
        binds = labelBinds program

    print $ runProgram binds insts
