import AdventOfCode.Intcode (computer, continueInput, output, readInputFileOrIntcode)

import Control.Monad (when)
import Data.Foldable (for_)

run :: Int -> [Int] -> [Int]
run inval mem = output (continueInput inval (computer mem))

main :: IO ()
main = do
  mem <- readInputFileOrIntcode

  let outputs = run 1 mem
  when (any (/= 0) (init outputs)) (putStrLn ("FAIL!!! " ++ show outputs))
  print (last outputs)

  for_ (run 5 mem) print
