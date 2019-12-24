import AdventOfCode (readInputFile)

import Control.Arrow (first)
import Data.Bits (popCount, shiftL, shiftR, testBit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl')
import qualified Data.Set as Set

sideLen, size :: Int
sideLen = 5
size = sideLen * sideLen

growBugs :: (Int -> [(Int, Int)]) -> IntMap Int -> IntMap Int
growBugs neigh grids = IntMap.fromList [(level, grid level) | level <- uniqueLevels]
  where nc = foldl' (Map.unionWith (+)) Map.empty (map (uncurry (neighCount neigh)) (IntMap.toList grids))
        uniqueLevels = nubConsec (map fst (Map.keys nc))
        grid level = sum (map (bit level) [0 .. (size - 1)])
        bit level i = if bitOn level i then 1 `shiftL` i else 0
        bitOn level i = case Map.lookup (level, i) nc of
          Just 1 -> True
          Just 2 -> not (IntMap.findWithDefault 0 level grids `testBit` i)
          _ -> False

neighCount :: (Int -> [(Int, Int)]) -> Int -> Int -> Map (Int, Int) Int
neighCount neigh level = count 0 Map.empty
  where count _ nc 0 = nc
        count pos nc n = count (pos + 1) nc' (n `shiftR` 1)
          where nc' = (if n `testBit` 0 then incrKeys keys else id) nc
                keys = map (first (+ level)) (neigh pos)
        incrKeys ks m = foldl' (\accm k -> Map.insertWith (+) k 1 accm) m ks

inBounds :: (Int, Int) -> Bool
inBounds (ny, nx) = good ny && good nx
  where good coord = 0 <= coord && coord < sideLen

neighFlat :: Int -> [(Int, Int)]
neighFlat pos =
  let (y, x) = pos `divMod` sideLen
      base = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
  in map (\(ny, nx) -> (0, ny * sideLen + nx)) (filter inBounds base)

neighRec :: Int -> [(Int, Int)]
neighRec pos =
  let (y, x) = pos `divMod` sideLen
      midCoord = sideLen `div` 2
      {- HLINT ignore neighRec "Use tuple-section" -}
      dirs = [
          (-1, 0, \nx -> (1, sideLen - 1,           nx)),
          (1, 0,  \nx -> (1,           0,           nx)),
          (0, -1, \ny -> (1,           ny, sideLen - 1)),
          (0, 1,  \ny -> (1,           ny,           0))
        ]
      level (dy, dx, innerNeigh) | y + dy == midCoord && x + dx == midCoord = map innerNeigh [0 .. (sideLen - 1)]
      level (dy, dx, _) | inBounds (ny, nx) = [(0, ny, nx)]
        where ny = y + dy
              nx = x + dx
      level (dy, dx, _) = [(-1, midCoord + dy, midCoord + dx)]
  in map (\(d, ny, nx) -> (d, ny * sideLen + nx)) (concatMap level dirs)

nubConsec :: Eq a => [a] -> [a]
nubConsec (x:y:xs) | x == y = x : nubConsec xs
nubConsec (x:xs) = x : nubConsec xs
nubConsec [] = []

repeated :: Ord a => (a -> a) -> a -> a
repeated f = rep Set.empty
  where rep seen x | x `Set.member` seen = x
        rep seen x = rep (Set.insert x seen) (f x)

bio1 :: Int -> Char -> Int
bio1 a '.' = 2 * a
bio1 a '#' = 2 * a + 1
bio1 a '\n' = a
bio1 _ c = error ("unknown " ++ [c])

main :: IO ()
main = do
  s <- readInputFile
  let bio = foldl' bio1 0 (reverse s)

  let growBugsFlat b = growBugs neighFlat (IntMap.singleton 0 b) IntMap.! 0
  print (repeated growBugsFlat bio)

  let iters = if bio == 1205552 then 10 else 200
      grids = iterate (growBugs neighRec) (IntMap.singleton 0 bio) !! iters
  print (sum (map popCount (IntMap.elems grids)))
