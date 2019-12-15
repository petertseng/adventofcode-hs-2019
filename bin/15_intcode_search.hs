{-# LANGUAGE TupleSections #-}

import AdventOfCode.Intcode (Computer(output), computer, continueInput, readInputFileOrIntcode)

import Data.Foldable (for_)
import Data.Maybe (mapMaybe)

search :: (a -> [a]) -> (a -> Bool) -> a -> (Int, [a])
search neigh goal start = search' 0 [start]
  where search' gen [] = (gen - 1, [])
        search' gen front | any goal front = (gen, filter goal front)
        search' gen front = search' (gen + 1) (concatMap neigh front)

-- can't use AdventOfCode.Search (bfs) here,
-- because my state is (Int, Computer) and Computer isn't Ord.
searchComp :: (Computer -> Bool) -> Computer -> (Int, [(Int, Computer)])
searchComp goal comp = search neighComp (goal . snd) (0, comp)

neighComp :: (Int, Computer) -> [(Int, Computer)]
neighComp (prevMove, comp) = mapMaybe (\dir -> fmap (dir,) (move comp dir)) (goodMoves prevMove)
  where goodMoves 1 = [1, 3, 4]
        goodMoves 2 = [2, 3, 4]
        goodMoves 3 = [1, 2, 3]
        goodMoves 4 = [1, 2, 4]
        goodMoves 0 = [1, 2, 3, 4]
        goodMoves n = error ("unknown move " ++ show n)

move :: Computer -> Int -> Maybe Computer
move comp dir = case output comp' of
  [0] -> Nothing
  [1] -> Just comp'
  [2] -> Just comp'
  x -> error ("expected one 0, 1, or 2, not " ++ show x)
  where comp' = continueInput dir (comp { output = [] })

main :: IO ()
main = do
  mem <- readInputFileOrIntcode

  let (len, comps) = searchComp (\c -> 2 `elem` output c) (computer mem)
  print len

  -- there should only be one, but it's fine
  for_ comps (print . fst . searchComp (const False) . snd)
