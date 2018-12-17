module Instruction
( Registers
, Instruction
, Program
, Args
, Op(..)
, Example(..)
, runOp
, readInput
, instOp
, instArgs
, ops
) where

import Control.Applicative ((<|>))
import Data.Bits (Bits((.&.),(.|.)))
import Data.Bool (bool)
import Data.Char (isDigit)
import Data.Function (const)
import Data.List (splitAt)
import Text.ParserCombinators.ReadP

type Registers = [Int]
type Instruction = (Int, Int, Int, Int)
type Program = [Instruction]
type Args = (Int, Int, Int)
type OpF = Args -> Registers -> Registers

data Op = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori
        | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr 
    deriving (Eq, Enum, Bounded)

data Example = Ex { before :: Registers
                  , inst   :: Instruction
                  , after  :: Registers
                  }

readReg :: ReadP Registers
readReg = do
    skipSpaces >> (string "Before:" <|> string "After:")
    skipSpaces >> (read <$> (manyTill get $ char '\n'))

readInst :: ReadP Instruction
readInst = do
    [a,b,c,d] <- count 4 $ munch (==' ') >> (read <$> many1 (satisfy isDigit))
    return (a,b,c,d)

readExample :: ReadP Example
readExample = do
    before <- skipSpaces >> readReg
    inst   <- skipSpaces >> readInst
    after  <- skipSpaces >> readReg
    return (Ex { before = before, inst = inst, after = after})

readExamples :: ReadP [Example]
readExamples = many1 readExample

readProgram :: ReadP Program
readProgram = skipSpaces >> sepBy1 readInst (char '\n')

readInput :: ReadP ([Example], Program)
readInput = do
    examples <- readExamples
    program  <- readProgram
    skipSpaces >> eof
    return (examples, program)

modReg :: Registers -> Int -> Int -> Registers
modReg reg i v = start ++ v:end
    where (start,_:end) = splitAt i reg

regOp :: (Int -> Int -> Int) -> OpF
imOpR :: (Int -> Int -> Int) -> OpF
imOpL :: (Int -> Int -> Int) -> OpF

regOp (-:-) (a,b,c) reg = modReg reg c ((reg!!a) -:- (reg!!b))
imOpR (-:-) (a,b,c) reg = modReg reg c ((reg!!a) -:- b)
imOpL (-:-) (a,b,c) reg = modReg reg c ((a)      -:- (reg!!b))

instOp   :: Instruction -> Int
instArgs :: Instruction -> Args

instOp   (o,_,_,_) = o
instArgs (_,a,b,c) = (a,b,c)

gtBool = (\a b -> bool 0 1 (a>b))
eqBool = (\a b -> bool 0 1 (a==b))

runOp :: Op -> OpF
runOp Addr = regOp (+)
runOp Addi = imOpR (+)
runOp Mulr = regOp (*)
runOp Muli = imOpR (*)
runOp Banr = regOp (.&.)
runOp Bani = imOpR (.&.)
runOp Borr = regOp (.|.)
runOp Bori = imOpR (.|.)
runOp Setr = regOp const
runOp Seti = imOpL const
runOp Gtir = imOpL gtBool
runOp Gtri = imOpR gtBool
runOp Gtrr = regOp gtBool
runOp Eqir = imOpL eqBool
runOp Eqri = imOpR eqBool
runOp Eqrr = regOp eqBool

ops :: [Op]
ops = enumFrom minBound
