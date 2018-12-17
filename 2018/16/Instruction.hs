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

digit = satisfy isDigit

readReg :: ReadP Registers
readReg = do
    skipSpaces >> (string "Before:" <|> string "After:")
    skipSpaces >> (read <$> (manyTill get $ char '\n'))

readInst :: ReadP Instruction
readInst = do
    [a,b,c,d] <- count 4 $ munch (==' ') >> (read <$> many1 digit)
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

addr = regOp (+)
addi = imOpR (+)
mulr = regOp (*)
muli = imOpR (*)
banr = regOp (.&.)
bani = imOpR (.&.)
borr = regOp (.|.)
bori = imOpR (.|.)

setr = regOp const
seti = imOpL const

gtBool = (\a b -> bool 0 1 (a>b))
gtir = imOpL gtBool
gtri = imOpR gtBool
gtrr = regOp gtBool

eqBool = (\a b -> bool 0 1 (a==b))
eqir = imOpL eqBool
eqri = imOpR eqBool
eqrr = regOp eqBool

runOp :: Op -> OpF
runOp Addr = addr
runOp Addi = addi
runOp Mulr = mulr
runOp Muli = muli
runOp Banr = banr
runOp Bani = bani
runOp Borr = borr
runOp Bori = bori
runOp Setr = setr
runOp Seti = seti
runOp Gtir = gtir
runOp Gtri = gtri
runOp Gtrr = gtrr
runOp Eqir = eqir
runOp Eqri = eqri
runOp Eqrr = eqrr


ops :: [Op]
ops = enumFrom minBound
