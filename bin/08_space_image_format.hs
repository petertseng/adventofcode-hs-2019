import AdventOfCode (readInputFile)

import Data.List (dropWhileEnd, find, intercalate, minimumBy, transpose)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where (first, rest) = splitAt n list

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

main :: IO ()
main = do
  s <- readInputFile
  let layers = splitEvery 150 (dropWhileEnd (== '\n') s)
  let minLayer = minimumBy (comparing (count '0')) layers
  print (count '1' minLayer * count '2' minLayer)

  let pixels = map (fromJust . find (/= '2')) (transpose layers)
  putStrLn (intercalate "\n" (splitEvery 25 (map (\c -> if c == '1' then '#' else ' ') pixels)))
