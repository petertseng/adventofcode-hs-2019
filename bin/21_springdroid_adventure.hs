import AdventOfCode.Intcode (Computer(output), computer, continueInputs, readInputFileOrIntcode)

import Data.Foldable (traverse_)

runSpringscript :: Computer -> String -> [Int]
runSpringscript comp str = filter (> 127) (output (continueInputs (map fromEnum str) comp))

main :: IO ()
main = do
  mem <- readInputFileOrIntcode
  let comp = computer mem

  traverse_ print (runSpringscript comp (unlines [
      "NOT C J"
    , "AND D J"
    , "NOT A T"
    , "OR T J"
    , "WALK"
    ]))

  traverse_ print (runSpringscript comp (unlines [
      "NOT H J"
    , "OR C J"
    , "AND A J"
    , "AND B J"
    , "NOT J J"
    , "AND D J"
    , "RUN"
    ]))
