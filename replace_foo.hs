module Main where
import System.Environment (getArgs)
import Data.List (isPrefixOf)
import System.IO (readFile')
import Data.Char (isSpace)

main :: IO ()
main = do
  [n', file] <- getArgs
  let n = read n' :: Int
  input <- readFile' file
  let (prefix, t1s:rest) = span prefixTest $ lines input
      (compute, suffix) = span  fooEndTest rest

  writeFile "body.ins" $ unlines compute
  writeFile file $ unlines $ prefix ++ [t1s, replacement n] ++ suffix

prefixTest :: String -> Bool
prefixTest = not . ("long t1s" `isPrefixOf`) . trimStart

fooEndTest :: String -> Bool
fooEndTest = ("struct timeval t2;" /=) . trimStart

replacement :: Int -> String
replacement n = unlines $ [ "\tfoo" ++ show i ++ "(x,y,val);"| i <- [10..n+9]]

trimStart :: String -> String
trimStart = dropWhile isSpace
