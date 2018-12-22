module Instruction
( Registers
, Instruction
, Program
, Line
, Args
, IPBind
, Op(..)
, runOp
, modReg
, parseInput
, instOp
, instArgs
, ops
) where

import Control.Applicative ((<|>))
import Control.Monad (liftM)
import Data.Bits (Bits((.&.),(.|.)))
import Data.Bool (bool)
import Data.Char (isDigit, toLower)
import Data.Function (const)
import Data.List (splitAt)
import Text.ParserCombinators.ReadP

type Registers = [Int]
type Instruction = (Op, Int, Int, Int)
type IPBind = Int
type Line = Either IPBind Instruction
type Program = [Line]
type Args = (Int, Int, Int)
type OpF = Args -> Registers -> Registers

data Op = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori
        | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr 
    deriving (Eq, Enum, Bounded, Show)

readInt :: ReadP Int
readInt = munch (==' ') >> (read <$> many1 (satisfy isDigit))

readInst :: ReadP Line
readInst = do
    skipSpaces
    let opNames = zip (map (map toLower . show) ops) ops
    opName <- choice $ map string $ map fst opNames
    let (Just op) = lookup opName opNames
    
    [b,c,d] <- count 3 readInt
    return $ Right (op,b,c,d)

readIP :: ReadP Line
readIP = skipSpaces >> string "#ip" >> liftM Left readInt

readLine :: ReadP Line
readLine = readInst <|> readIP

readProgram :: ReadP Program
readProgram = skipSpaces >> sepBy1 readLine (char '\n')

readInput :: ReadP Program
readInput = do
    program  <- readProgram
    skipSpaces >> eof
    return program

parseInput :: String -> Program
parseInput = fst . last . readP_to_S readInput

modReg :: Registers -> Int -> Int -> Registers
modReg reg i v = start ++ v:end
    where (start,_:end) = splitAt i reg

regOp :: (Int -> Int -> Int) -> OpF
imOpR :: (Int -> Int -> Int) -> OpF
imOpL :: (Int -> Int -> Int) -> OpF

regOp (-:-) (a,b,c) reg = modReg reg c ((reg!!a) -:- (reg!!b))
imOpR (-:-) (a,b,c) reg = modReg reg c ((reg!!a) -:- b)
imOpL (-:-) (a,b,c) reg = modReg reg c ((a)      -:- (reg!!b))

instOp   :: Instruction -> Op
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
