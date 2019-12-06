import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)
import AdventOfCode.Split (splitOnOne)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)

tup2ls :: (a, a) -> [(a, [a])]
tup2ls (a, b) = [(b, [a]), (a, [b])]

depth :: Map String String -> String -> Int
depth orbit x = case Map.lookup x orbit of
  Nothing -> 0
  Just y -> 1 + depth orbit y

main :: IO ()
main = do
  s <- readInputFile
  let orbit = map (swap . splitOnOne ')') (lines s)
      transfer = Map.fromListWith (++) (concatMap tup2ls orbit)

  print (sum (map (depth (Map.fromList orbit) . fst) orbit))

  let printGoal (Right (gen, _):_) = print gen
      printGoal _ = putStrLn "impossible"
      neigh = flip (Map.findWithDefault []) transfer

  case (lookup "YOU" orbit, lookup "SAN" orbit) of
    (Just youOrbit, Just sanOrbit) -> printGoal (bfs neigh (== sanOrbit) youOrbit)
    _ -> putStrLn "nonexistent"
