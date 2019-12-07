import AdventOfCode.Intcode (Computer, computer, continueInputs, continueInput, halt, output, readInputFileOrIntcode)

import Data.Foldable (for_)
import Data.List (permutations)

data Queue a = Queue [a] [a]

push :: a -> Queue a -> Queue a
push e (Queue inb out) = Queue (e:inb) out

pop :: Queue a -> (a, Queue a)
pop (Queue [] []) = error "never happens in day 7"
pop (Queue inb []) = pop (Queue [] (reverse inb))
pop (Queue inb (x:xs)) = (x, Queue inb xs)

chainStep :: [Int] -> Queue Computer -> [Int]
chainStep prev computers = if halt first then prev
  else chainStep (output first') (push first' computers')
  where (first, computers') = pop computers
        first' = continueInputs prev (first { output = [] })

chain :: [Int] -> [Int] -> Int
chain mem phases = case chainStep [0] computers of
  [x] -> x
  l -> error ("unexpected final output " ++ show l)
  where computers = Queue [] (map compWithPhase phases)
        compWithPhase phase = continueInput phase (computer mem)

main :: IO ()
main = do
  mem <- readInputFileOrIntcode
  for_ [[0..4], [5..9]] (print . maximum . map (chain mem) . permutations)
