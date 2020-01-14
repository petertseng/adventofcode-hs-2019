import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)
import AdventOfCode.Split (splitOnOne)

import Data.Either (rights)
import qualified Data.Map as Map
import Data.Tuple (swap)

tup2ls :: (a, a) -> [(a, [a])]
tup2ls (a, b) = [(b, [a]), (a, [b])]

main :: IO ()
main = do
  s <- readInputFile
  let orbit = map (swap . splitOnOne ')') (lines s)
      transfer = Map.fromListWith (++) (concatMap tup2ls orbit)
      neigh = flip (Map.findWithDefault []) transfer

  print . sum . map fst . rights $ bfs neigh (const True) "COM"

  let printGoal (Right (gen, _):_) = print gen
      printGoal _ = putStrLn "impossible"

  case (lookup "YOU" orbit, lookup "SAN" orbit) of
    (Just youOrbit, Just sanOrbit) -> printGoal (bfs neigh (== sanOrbit) youOrbit)
    _ -> putStrLn "nonexistent"
