{- 
program: memcon
author:  Colin Woodbury
concact: <colingw@gmail.com>
usage:   
 -a <num + denom>
  "All." Gives _all_ conversions of the given value from Byte to Yottabyte.
 -u <num + denom>
  "Up denomination." Gives the value as a higher denomination. E.g. Byte -> kB
 -d <num + denom>
  "Down denomination." Gives the value as a lower denomination. E.g. mB -> kB
-}

import Data.List (elemIndex, intersperse)
import ColinPrelude (groupsOfN, list)
import System.Environment (getArgs)
import System.Console.GetOpt

data Flag = UpDenom | DownDenom | AllDenoms

-- CONSTANT FUNCTIONS
options :: [OptDescr Flag]
options =
    [ Option ['u'] ["updenom"]   (NoArg UpDenom)   upDesc
    , Option ['d'] ["downdenom"] (NoArg DownDenom) downDesc
    , Option ['a'] ["alldenoms"] (NoArg AllDenoms) allDesc
    ]
    where
      upDesc   = "Gives the next higher denomination of the input."
      downDesc = "Gives the next lower denomination of the input."
      allDesc  = "Gives all conversions of the input."

-- 1024 bytes to kB, 1024 kBs to a mB, etc.
perDenom :: Integral a => a
perDenom = 1024

-- Only goes up to yottabyte (2 ^ 80 bytes)
denoms :: String
denoms = "kmgtpezy"

highestDenom :: String
highestDenom = [last denoms,'B']

lowestDenom :: String
lowestDenom = "B"

-- THE WORK
main = do
  input <- getArgs
  opts  <- processOpts input
  case id opts of
    ([UpDenom],  [memory]) -> putStrLn $ upDenom memory
    ([DownDenom],[memory]) -> putStrLn $ downDenom memory
    ([AllDenoms],[memory]) -> putStrLn $ getAllDenoms memory

-- Gets and processes command line flags.        
processOpts :: [String] -> IO ([Flag],[String])
processOpts args = 
    case getOpt Permute options args of
      ([],_,errors)     -> optError "No flag given!" errors
      (_,[],errors)     -> optError "No memory amount given!" errors
      (opts,nonopts,[]) -> return (opts,nonopts)

optError :: String -> [String] -> IO a          
optError message errors = ioError (userError (concat errors ++ usageInfo msg options))
    where
      msg    = message ++ "\n" ++ header
      header = "Usage : memcon [OPTION] \"<memory amount>\""

-- Given a data amount, yields all conversions of it from Byte to Yottabyte.
-- Commas are added as necessary for easy reading by humans.
getAllDenoms :: String -> String
getAllDenoms = unlines . getAllDownDenoms . getAllUpDenoms' . list . addCommas'
    where
      addCommas' = \x -> if ',' `notElem` x then addCommas x else x
      getAllUpDenoms' = reverse . getAllUpDenoms

getAllUpDenoms :: [String] -> [String]
getAllUpDenoms [] = []
getAllUpDenoms orig@(d:ds) | getLabelPart d == highestDenom = orig
                           | otherwise = getAllUpDenoms $ upDenom d : orig

getAllDownDenoms :: [String] -> [String]
getAllDownDenoms [] = []
getAllDownDenoms orig@(d:ds) | getLabelPart d == lowestDenom = orig
                             | otherwise = getAllDownDenoms $ downDenom d : orig
                                        
upDenom :: String -> String
upDenom line = changeDenom downNum upLabel line

downDenom :: String -> String
downDenom line = changeDenom upNum downLabel line

changeDenom :: (Integral a, Read a) => (a -> a) -> (String -> String) -> String -> String
changeDenom changeNum changeLabel line = addCommas $ newNum ++ " " ++ newLabel
    where 
      num      = filterCommas $ getNumPart line
      newNum   = show $ changeNum $ read num
      newLabel = changeLabel $ getLabelPart line

addCommas :: String -> String
addCommas memory = (addCommasToNum numPart) ++ " " ++ labelPart
    where
      numPart = read $ getNumPart memory
      labelPart = getLabelPart memory

getNumPart :: String -> String
getNumPart = head . words

getLabelPart :: String -> String
getLabelPart = last . words

addCommasToNum :: Integral a => a -> String
addCommasToNum = reverse . concat . intersperse "," . groupsOfN 3 . reverse . show

filterCommas :: String -> String
filterCommas = filter (\c -> not $ c == ',')

upNum :: Integral a => a -> a
upNum n = n * perDenom

downNum :: Integral a => a -> a
downNum n = n `div` perDenom

alterLabel :: String -> String -> String
alterLabel x ds = case elemIndex (head x) ds of
                   Just i  -> ds !! (i + 1) : "B"
                   Nothing -> error $ "Invalid denomination given -> " ++ x

upLabel :: String -> String
upLabel lab | head lab == last denoms = error $ "Cannot go higher than " ++ lab
            | head lab == 'B' = "kB"
            | otherwise = alterLabel lab denoms

downLabel :: String -> String
downLabel lab | head lab == 'B' = error "Cannot go lower than a byte!"
              | head lab == 'k' = "B"
              | otherwise = alterLabel lab $ reverse denoms

