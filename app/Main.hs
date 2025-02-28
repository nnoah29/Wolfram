{-
-- EPITECH PROJECT, 2025
-- $WOLFRAM
-- File description:
-- $WOLFRAM
-}

module Main (main) where
import Lib ()
import Rules
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)


data Config = Config {
    rule :: Int,
    start :: Int,
    nbLines :: Maybe Int,
    window :: Int,
    move :: Int
} deriving (Show)

defaultConfig :: Config
defaultConfig = Config {
    rule = -1,
    start = 0,
    nbLines = Nothing,
    window = 80,
    move = 0
}

exitWithError :: String -> IO a
exitWithError msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

getConfig :: [String] -> Config -> IO Config
getConfig [] config =
    if rule config == -1 then exitWithError "--rule is required"
    else return config
getConfig ("--rule"  : r : xs) config =
    case readMaybe r of
        Just val -> getConfig xs config { rule = val }
        Nothing  -> exitWithError "Invalid value for --rule"
getConfig ("--start" : s : xs) config =
    case readMaybe s of
        Just val | val >= 0 -> getConfig xs config { start = val }
        _ -> exitWithError "Invalid value for --start (must be >= 0)"
getConfig ("--lines" : l : xs) config =
    case readMaybe l of
        Just val | val > 0 -> getConfig xs config { nbLines = Just val }
        _ -> exitWithError "Invalid value for --lines (must be > 0)"
getConfig ("--window": w : xs) config =
    case readMaybe w of
        Just val | val > 0 -> getConfig xs config { window = val }
        _ -> exitWithError "Invalid value for --window (must be > 0)"
getConfig ("--move"  : m : xs) config =
    case readMaybe m of
        Just val -> getConfig xs config { move = val }
        Nothing  -> exitWithError "Invalid value for --move"
getConfig (_ : _) _ = exitWithError "Invalid argument"


initRows:: Int -> [Bool]
initRows a = [if i == (a `div` 2) then True else False | i <- [0..a-1]]
  
displayRow :: Config -> [Bool] -> IO ()
displayRow conf row =
    let x =  ((length row) - window conf) `div` 2
        y = length row - x
        spaces = replicate (max 0 (move conf)) ' '
        display =
            if move conf >= 0
            then [if cell then '*' else ' ' | cell <- drop x (take y row)]
            else [if cell then '*' else ' ' | cell <- drop (x - move conf) (take (y) row)]
    in putStrLn (spaces ++ display)



nextGen :: [Bool] -> Config -> [Bool]
nextGen row conf =
    let left  = (False:(init row))
        right = ((drop 1 row) ++ [False])
        in [applyRule (rule conf) l c r | (l, c, r) <- zip3 left row right]


generate :: [Bool] -> Int -> Config -> IO ()
generate prev currentGen conf
    | currentGen < (start conf) =
        generate (nextGen ([False, False] ++ prev ++ [False, False]) conf) (currentGen + 1) conf
    | otherwise = case nbLines conf of
        Just 0 -> return ()
        Just n ->
            let row = nextGen ([False, False] ++ prev ++ [False, False]) conf
            in displayRow conf row >>
            generate row (currentGen + 1) conf {nbLines = Just (n - 1)}
        Nothing ->
            let row = nextGen ([False, False] ++ prev ++[False, False]) conf
            in displayRow conf row >>
            generate row (currentGen + 1) conf
            
wolfram2 :: Config -> [Bool] -> IO ()
wolfram2 conf row = if ((start conf == 0 && nbLines conf == Nothing) ) then displayRow conf row >>
        generate row 1 conf
    else generate row 1 conf
  
wolfram :: Config -> IO ()
wolfram conf =
    let row = initRows (window conf)
        n = fromMaybe 1 (nbLines conf)
    in if  ((start conf == 0 && nbLines conf /= Nothing) ) then displayRow conf row >>
        generate row 1 conf {nbLines = Just (n - 1)}
    else wolfram2 conf row

-------------------------------------------------

ruleSupported:: Int -> Bool
ruleSupported 30 = True
ruleSupported 90 = True
ruleSupported 110 = True
ruleSupported _ = False


main :: IO ()
main = do
    args <- getArgs
    conf <- getConfig args defaultConfig
    if (ruleSupported (rule conf)) then wolfram conf
    else exitWithError "Rule is not supported"
    
-------------------------------------------------
--- ((length row) - window conf) `div` 2