module Army
( Group(..)
, Army(getArmy)
) where

import Control.Monad (liftM, liftM2)
import Data.Char (isDigit, isAlpha)
import Text.ParserCombinators.ReadP
import Text.Read(Read(readsPrec))
import qualified Data.Map as M

data Group = Group { gArmy       :: String
                   , gUnits      :: Int
                   , gHp         :: Int
                   , gMods       :: M.Map String [String]
                   , gDamage     :: Int
                   , gDamageType :: String
                   , gInitiative :: Int
                   } deriving (Show, Eq)

newtype Army = Army { getArmy :: [Group] } deriving Show

instance Read Army where
    readsPrec _ = readP_to_S readArmy

readInt = read <$> munch isDigit
readWord = munch isAlpha

commaSep1 = sepBy1 readWord (string ", ")

readMod :: ReadP (String, [String])
readMod = liftM2 (,) readWord $ string " to " >> commaSep1

readMods :: ReadP (M.Map String [String])
readMods = liftM M.fromList $ sepBy1 readMod $ string "; "

readGroup :: String -> ReadP Group
readGroup armyName = do
    units      <- readInt
    string " units each with "
    hp         <- readInt
    string " hit points "
    mods       <- (between (char '(') (string ") ") readMods) <++ return M.empty
    string "with an attack that does "
    damage     <- readInt
    char ' '
    damageType <- readWord
    string " damage at initiative "
    initiative <- readInt
    return $ Group armyName units hp mods damage damageType initiative

readArmy :: ReadP Army
readArmy = do
    armyName <- munch (/= ':')
    char ':' >> skipSpaces
    groups <- sepBy1 (readGroup armyName) (char '\n')
    skipSpaces
    return $ Army groups
