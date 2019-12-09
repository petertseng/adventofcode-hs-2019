import AdventOfCode.Intcode (computer, continueInput, output, readInputFileOrIntcode)

run :: Int -> [Int] -> [Int]
run inval mem = output (continueInput inval (computer mem))

main :: IO ()
main = do
  mem <- readInputFileOrIntcode
  mapM_ print (run 1 mem)
  mapM_ print (run 2 mem)
