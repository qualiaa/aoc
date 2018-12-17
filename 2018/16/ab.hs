{-# LANGUAGE RecordWildCards #-}

import Instruction
import Data.List (foldl', intersect, (\\))
import qualified Data.Map.Strict as M

import Text.ParserCombinators.ReadP (readP_to_S)

equivOps :: Example -> [Op]
equivOps Ex{..} = filter (\op -> runOp op (instArgs inst) before == after) ops

possibleOps :: [Example] -> M.Map Int [Op]
possibleOps = foldl' restrict initMap
    where initMap = M.fromList . zip [0..15] $ repeat ops
          restrict m e @ (Ex {inst = inst}) =
                M.insertWith intersect (instOp inst) (equivOps e) m

uniqueOpcodes :: M.Map Int [Op] -> M.Map Int Op
uniqueOpcodes m
    | null m = M.empty
    | otherwise = M.union uniqs (uniqueOpcodes m')
    where uniqs = M.map head $ M.filter ((==1) . length) m
          m' = M.map (\\ M.elems uniqs) . M.withoutKeys m $ M.keysSet uniqs

runProgram :: M.Map Int Op -> Program -> Registers
runProgram opcodes = foldl' runOp' initReg
    where initReg = replicate 4 0
          runOp' reg inst = runOp (opcodes M.! instOp inst) (instArgs inst) reg

main = do
    (examples, program) <- fst . last . readP_to_S readInput <$> getContents
    
    print . length . filter ((>=3) . length) $ map equivOps examples

    let opcodes = uniqueOpcodes $ possibleOps examples

    print $ runProgram opcodes program
