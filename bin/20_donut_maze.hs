import AdventOfCode (readInputFile)
import AdventOfCode.Search (astar, bfs)

import Data.Array.Unboxed ((!), UArray, array, bounds)
import Data.Char (isUpper)
import Data.Either (rights)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int)
type Maze = UArray Pos Char
type Teleporters = Map Pos [(Int, Pos, Int -> Int)]

neigh :: Teleporters -> ((Int -> Int) -> (Int -> Int)) -> (Pos, Int) -> [(Int, (Pos, Int))]
neigh teles ffDepth (pos, depth) = filter goodDepth moves
  where goodDepth (_, (_, d)) = 0 <= d && d <= maxDepth
        portalPairs = (Map.size teles - 2) `div` 2
        maxDepth = ((portalPairs + 1) `div` 2) * (portalPairs `div` 2)
        moves = map move (teles Map.! pos)
        move (dist, pos', fDepth) = (dist, (pos', ffDepth fDepth depth))

mazeNeigh1 :: Maze -> Pos -> [Pos]
mazeNeigh1 maze pos = filter canMove (map (step pos) dirs)
  where canMove pos' = maze ! pos' == '.'

portalToPortal :: Maze -> [Pos] -> [(Pos, [(Int, Pos, Int -> Int)])]
portalToPortal maze portals = zip portals (map findPortals portals)
  where portalSet = Set.fromList portals
        findPortals portal =
          map sameDepth . rights $ bfs (mazeNeigh1 maze) (`Set.member` portalSet) portal
        sameDepth (a, b) = (a, b, id)

parseMaze :: [(Pos, Char)] -> Maze -> (Pos, Pos, Teleporters)
parseMaze letters maze =
  let ((minY, minX), (maxY, maxX)) = bounds maze
      makePair ((y, _), _) | y == minY || y == maxY = Nothing
      makePair ((_, x), _) | x == minX || x == maxX = Nothing
      makePair (pos, letter) = case find (\dir -> maze ! step pos dir == '.') dirs of
        Nothing -> Nothing
        Just dotDir ->
          let otherLetterDir = opposite dotDir
              otherLetterPos@(oly, olx) = step pos otherLetterDir
              isOuter = oly == minY || oly == maxY || olx == minX || olx == maxX
              otherLetter = maze ! otherLetterPos
              (l1, l2) = if negDir otherLetterDir then (otherLetter, letter) else (letter, otherLetter)
          in Just ((isOuter, l1, l2), step pos dotDir)
      pairs :: [((Bool, Char, Char), Pos)]
      pairs = mapMaybe makePair letters
      pairsMap = Map.fromList pairs
      toTele ((_, 'A', 'A'), _) = Nothing
      toTele ((_, 'Z', 'Z'), _) = Nothing
      toTele ((isOuter, a, b), pos) =
        let otherPos = pairsMap Map.! (not isOuter, a, b) in
        Just (pos, [(1, otherPos, if isOuter then pred else succ)])
      teles = Map.fromListWith (error "duplicate teleporter") (mapMaybe toTele pairs)
  in (pairsMap Map.! (True, 'A', 'A'), pairsMap Map.! (True, 'Z', 'Z'), teles)

dirs :: [Dir]
dirs = [Dir (-1, 0), Dir (1, 0), Dir (0, -1), Dir (0, 1)]

negDir :: Dir -> Bool
negDir (Dir (dy, dx)) = dy < 0 || dx < 0

opposite :: Dir -> Dir
opposite (Dir (dy, dx)) = Dir (-dy, -dx)

step :: Pos -> Dir -> Pos
step (a, b) (Dir (c, d)) = (a + c, b + d)

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [1..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [1..]

main :: IO ()
main = do
  s <- readInputFile
  let grid = enumGrid (lines s)
      height = maximum (map (fst . fst) grid)
      width = maximum (map (snd . fst) grid)
      maze = array ((1, 1), (height, width)) grid
      letters = filter (isUpper . snd) grid
      (start, goal, teles) = parseMaze letters maze
      p2p = portalToPortal maze (start : goal : Map.keys teles)
      dists = Map.unionWith (++) (Map.fromList p2p) teles
      printMaybe = putStrLn . maybe "impossible" show
      putSearch f = printMaybe $ astar (neigh dists f) (const 0) (== (goal, 0)) (start, 0)

  putSearch (const id)
  putSearch id
