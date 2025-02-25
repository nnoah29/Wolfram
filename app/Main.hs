{-
-- EPITECH PROJECT, 2025
-- $WOLFRAM
-- File description:
-- $WOLFRAM
-}

module Main (main) where
import Lib ()
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)


data Config = Config {
    rule :: Int,
    start :: Int,
    nbLines :: Maybe Int,
    window :: Int,
    move :: Int
} deriving (Show)

defaultConfig :: Config
defaultConfig = Config {
    rule = error "--rule is required",
    start = 0,
    nbLines = Nothing,
    window = 80,
    move = 0
}

getConfig :: [String] -> Config -> Config
getConfig [] config = config
getConfig ("--rule"  : r : xs) config =
    getConfig xs config {rule = read r }
getConfig ("--start" : s : xs) config =
    getConfig xs config {start = fromMaybe 0 (readMaybe s)}
getConfig ("--lines" : l : xs) config =
    getConfig xs config {nbLines = readMaybe l }
getConfig ("--window": w : xs) config =
    getConfig xs config {window = fromMaybe 80 (readMaybe w)}
getConfig ("--move"  : m : xs) config =
    getConfig xs config {move = fromMaybe 0 (readMaybe m)}
getConfig (_ : xs) config = getConfig xs config


initRows:: Int -> [Bool]
initRows a = [if i == (a `div` 2) then True else False | i <- [0..a-1]]
  
displayRow :: [Bool] -> IO ()
displayRow row = putStrLn [if cell == True then '*' else ' ' |
 cell <- drop 500 (take (length row - 500) row)]

applyRule :: Int -> Bool -> Bool -> Bool -> Bool
applyRule  30 l c r = rule30  l c r
applyRule  90 l c r = rule90  l c r
applyRule 110 l c r = rule110 l c r
applyRule _ _ _ _ = False

nextGen :: [Bool] -> Config -> [Bool]
nextGen row conf =
    let left  = (False:(init row))
        right = ((drop 1 row) ++ [False])
        in [applyRule (rule conf) l c r | (l, c, r) <- zip3 left row right]


generate :: [Bool] -> Int -> Config -> IO ()
generate prev currentGen conf
    | currentGen < (start conf) =
        generate (nextGen prev conf) (currentGen + 1) conf
    | otherwise = case nbLines conf of
        Just 0 -> return ()
        Just n ->
            let row = nextGen prev conf
            in displayRow row >>
            generate row (currentGen + 1) conf {nbLines = Just (n - 1)}
        Nothing ->
            let row = nextGen prev conf
            in displayRow row >>
            generate row (currentGen + 1) conf
  
wolfram :: Config -> IO ()
wolfram conf =
    let row = initRows (window conf + 1000)
        n = fromMaybe 1 (nbLines conf)
    in if  (start conf == 0) then displayRow row >>
        generate row 1 conf {nbLines = Just (n - 1)}
    else generate row 1 conf

rule30:: Bool -> Bool -> Bool -> Bool
rule30 True  True  True  = False -- 111 = 0
rule30 True  True  False = False -- 110 = 0
rule30 True  False True  = False -- 101 = 0
rule30 True  False False = True  -- 100 = 1
rule30 False True  True  = True  -- 011 = 1
rule30 False True  False = True  -- 010 = 1
rule30 False False True  = True  -- 001 = 1
rule30 False False False = False -- 000 = 0


rule90:: Bool -> Bool -> Bool -> Bool
rule90 True  True  True  = False -- 111 = 0
rule90 True  True  False = True  -- 110 = 1
rule90 True  False True  = False -- 101 = 0
rule90 True  False False = True  -- 100 = 1
rule90 False True  True  = True  -- 011 = 1
rule90 False True  False = False -- 010 = 0
rule90 False False True  = True  -- 001 = 1
rule90 False False False = False -- 000 = 0


rule110:: Bool -> Bool -> Bool -> Bool
rule110 True  True  True  = False -- 111 = 0
rule110 True  True  False = True  -- 110 = 1
rule110 True  False True  = True  -- 101 = 1
rule110 True  False False = False -- 100 = 0
rule110 False True  True  = True  -- 011 = 1
rule110 False True  False = True  -- 010 = 1
rule110 False False True  = True  -- 001 = 1
rule110 False False False = False -- 000 = 0

main :: IO ()
main = do
    args <- getArgs
    let conf = getConfig args defaultConfig in wolfram conf

----------------------------------------
