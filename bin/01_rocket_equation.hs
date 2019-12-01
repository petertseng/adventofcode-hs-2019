import AdventOfCode (readInputFile)

fuel :: Integral a => a -> a
fuel mass = mass `div` 3 - 2

fuelOfFuel :: Integral a => a -> a
fuelOfFuel = sum . takeWhile (>= 0) . drop 1 . iterate fuel

main :: IO ()
main = do
  s <- readInputFile
  let masses = map read (lines s) :: [Int]
      printSum = print . sum . flip map masses
  printSum fuel
  printSum fuelOfFuel
