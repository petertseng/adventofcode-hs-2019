import AdventOfCode.Intcode (Computer(output), computer, continueInputs, readInputFileOrIntcode)

import Data.List (find)
import Data.Maybe (fromJust)

find100 :: Computer -> Int -> Int -> Int
find100 comp y minX = let minX' = fromJust (find (\x -> pull comp (y, x)) [minX..])
  in if pull comp (y - 99, minX' + 99) then minX' * 10000 + y - 99 else find100 comp (y + 1) minX'

pull :: Computer -> (Int, Int) -> Bool
pull comp (y, x) = case output (continueInputs [x, y] comp) of
  [1] -> True
  [0] -> False
  l -> error ("unexpected output " ++ show l)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  mem <- readInputFileOrIntcode
  let comp = computer mem
  print (count (pull comp) [(y, x) | x <- [0..49], y <- [0..49]])
  print (find100 comp 99 0)
