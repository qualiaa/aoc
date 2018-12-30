{-# LANGUAGE RecordWildCards #-}

import Instruction
import Data.List (foldl', lookup)
import qualified Data.Set as S
import Data.Either (rights)
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Writer.Lazy

type Binds = M.Map Int IPBind
type Insts = M.Map Int Instruction
type Trace = [(Int, Registers)]

labelAddresses :: Program -> M.Map Int Instruction
labelAddresses = M.fromList . zip [0..] . rights

labelBinds :: Program -> M.Map Int IPBind
labelBinds = fst . foldl' label (M.empty, 0)
    where label (m,i) (Left ip) = (M.insert i ip m, i)
          label (m,i) (Right _) = (m, succ i)

runInstruction :: Instruction -> Registers -> Registers
runInstruction (op, a, b, c) = runOp op (a,b,c)

runProgram :: Binds -> Insts -> Writer Trace Registers
runProgram binds insts = runFromLine 0 $ replicate 6 0


    where runFromLine :: Int -> Registers -> Writer Trace Registers
          runFromLine line reg = do
                case (insts M.!? line) of
                    Nothing -> return reg
                    Just inst -> do
                        let reg' = runInstruction inst reg
                            (line', reg'') = findNextLine binds line reg'
                        tell [(line,reg')]
                        runFromLine line' reg''

findNextLine :: Binds -> Int -> Registers -> (Int, Registers)
findNextLine binds line reg = maybe (line+1, reg) updateIP (snd <$> M.lookupLE line binds)
    where updateIP ipBind = let nextLine = (reg !! ipBind) + 1
                            in (nextLine, modReg reg ipBind nextLine)

findLastVal :: [Int] -> Int
findLastVal = findLastVal' []
    where findLastVal' seen (x:xs)
             | x `elem` seen = x
             | otherwise = findLastVal' (x:seen) xs

main = do
    program <- parseInput <$> getContents
    let insts = labelAddresses program
        binds = labelBinds program

    let trace = execWriter $ runProgram binds insts
    let vals = map ((!!3) . snd) $ filter ((28==) . fst) trace

    print $ head vals

    print $ vals !! 10310

    print $ findLastVal vals
