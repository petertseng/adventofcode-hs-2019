import AdventOfCode.Intcode (computer, continueDefault, memRead, readInputFileOrIntcode)

runNV :: [Int] -> Int -> Int -> Int
runNV [] _ _ = error "no program to run"
runNV (x:xs) noun verb = memRead 0 . continueDefault . computer $ mem
  where mem = x : noun : verb : drop 2 xs

nvFromDeltas :: Int -> Int -> Int -> Int -> (Int, Int)
nvFromDeltas target base deltaNoun deltaVerb =
  if deltaNoun > deltaVerb then
    let noun = (target - base) `div` deltaNoun
    in (noun, (target - base - deltaNoun * noun) `div` deltaVerb)
  else
    let verb = (target - base) `div` deltaVerb
    in ((target - base - deltaVerb * verb) `div` deltaNoun, verb)

main :: IO ()
main = do
  mem <- readInputFileOrIntcode
  let run = runNV mem
  print $ run 12 2

  let target = 19690720
      base = run 0 0
      deltaNoun = run 1 0 - base
      deltaVerb = run 0 1 - base
  let (noun, verb) = nvFromDeltas target base deltaNoun deltaVerb
  print $ noun * 100 + verb
