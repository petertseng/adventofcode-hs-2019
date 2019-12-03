import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.Either (partitionEithers)
import Data.List (mapAccumL)
import Data.Maybe (catMaybes)

data Dir = U | D | L | R deriving (Read, Show)

type Segment = (Int, Int, Int, Int, Int)
type Wire = ([Segment], [Segment])

wireSegment :: (Int, Int, Int) -> (Dir, Int) -> ((Int, Int, Int), Either Segment Segment)
wireSegment (y, x, totLen) (dir, len) = let totLen' = totLen + len in
  case dir of
  U -> let y' = y - len in ((y', x, totLen'), Left (x, y', y, totLen', -1))
  D -> let y' = y + len in ((y', x, totLen'), Left (x, y, y', totLen, 1))
  L -> let x' = x - len in ((y, x', totLen'), Right (y, x', x, totLen', -1))
  R -> let x' = x + len in ((y, x', totLen'), Right (y, x, x', totLen, 1))

wireSegments :: [(Dir, Int)] -> Wire
wireSegments = partitionEithers . snd . mapAccumL wireSegment (0, 0, 0)

intersectWires :: Wire -> Wire -> [(Int, Int)]
intersectWires (vert1, horiz1) (vert2, horiz2) = intersectSegs vert1 horiz2 ++ intersectSegs vert2 horiz1

intersectSegs :: [Segment] -> [Segment] -> [(Int, Int)]
intersectSegs verts horizs = catMaybes [v `intersect` h | v <- verts, h <- horizs]

intersect :: Segment -> Segment -> Maybe (Int, Int)
intersect (y, _, _, _, _) (_, ymin, ymax, _, _) | ymin > y || y > ymax = Nothing
intersect (_, xmin, xmax, _, _) (x, _, _, _, _) | xmin > x || x > xmax = Nothing
intersect (0, _, _, _, _) (0, _, _, _, _) = Nothing
intersect (y, xmin, _, l1min, l1d) (x, ymin, _, l2min, l2d) = Just (abs y + abs x, l1 + l2)
  where l1 = l1min + l1d * (x - xmin)
        l2 = l2min + l2d * (y - ymin)

parse :: String -> (Dir, Int)
parse (letter:dist) = (read [letter], read dist)
parse "" = error "empty"

main :: IO ()
main = do
  s <- readInputFile
  let wires = map (wireSegments . map parse . splitOn ',') (lines s)
      (wire1, wire2) = case wires of
        [a, b] -> (a, b)
        _ -> error ("expected two wires not " ++ show (length wires))
      (fromOrigin, onWire) = unzip (intersectWires wire1 wire2)

  print (minimum fromOrigin)
  print (minimum onWire)
