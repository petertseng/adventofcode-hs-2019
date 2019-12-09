import AdventOfCode.Intcode (computer, continueInput, output, readInputFileOrIntcode)

import Data.Foldable (for_)

run :: Int -> [Int] -> [Int]
run inval mem = output (continueInput inval (computer mem))

main :: IO ()
main = do
  mem <- readInputFileOrIntcode
  for_ (run 1 mem) print
  for_ (run 2 mem) print
