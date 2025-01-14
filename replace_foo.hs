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
  let (prefix, rest) = break fooPrefixTest $ lines input
      (foos, suffix) = span  fooPrefixTest rest

  writeFile file $ unlines $ prefix ++ [replacement n] ++ suffix

fooPrefixTest :: String -> Bool
fooPrefixTest = ("foo" `isPrefixOf`) . trimStart

replacement :: Int -> String
replacement n = unlines $ [ "  foo" ++ show i ++ "(x,y,val);"| i <- [10..n+9]]

trimStart :: String -> String
trimStart = dropWhile isSpace
