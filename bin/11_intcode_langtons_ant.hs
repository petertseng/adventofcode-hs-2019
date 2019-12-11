import AdventOfCode.Intcode (Computer(halt, output), computer, continueInput, readInputFileOrIntcode)

import Control.Arrow (first)
import Data.Foldable (for_)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int)
type BotAndGrid = (Pos, Dir, Set Pos, Set Pos)

pairs :: [a] -> ([(a, a)], [a])
pairs (a:b:xs) = first ((a, b) :) (pairs xs)
pairs xs = ([], xs)

run :: Int -> Computer -> ([(Int, Int)], Computer)
run inval comp = (outPairs, comp' { output = out' })
  where comp' = continueInput inval comp
        (outPairs, out') = pairs (output comp')

left :: Dir -> Dir
-- (-1, 0) -> (0, -1) -> (1, 0) -> (0, 1) -> (-1, 0)
left (Dir (dy, dx)) = Dir (-dx, dy)

right :: Dir -> Dir
right = left . left . left

step :: Pos -> Dir -> Pos
step (a, b) (Dir (c, d)) = (a + c, b + d)

paint :: [Int] -> Bool -> (Set Pos, Set Pos)
paint mem startsWhite = paint' (computer mem) ((0, 0), Dir (-1, 0), white, Set.empty)
  where white = if startsWhite then Set.singleton (0, 0) else Set.empty

paint' :: Computer -> BotAndGrid -> (Set Pos, Set Pos)
paint' comp (_, _, w, v) | halt comp = (w, v)
paint' comp botAndGrid@(pos, _, white, _) = paint' comp' botAndGrid'
  where (outPairs, comp') = run cameraInput comp
        cameraInput = fromEnum (pos `Set.member` white)
        botAndGrid' = foldl' move botAndGrid outPairs

move :: BotAndGrid -> (Int, Int) -> BotAndGrid
move (pos, dir, white, visit) (paintWhite, turn) = (pos', dir', white', visit')
  where white' = (if toEnum paintWhite then Set.insert else Set.delete) pos white
        dir' = (if toEnum turn then right else left) dir
        pos' = step pos dir'
        visit' = Set.insert pos' visit

main :: IO ()
main = do
  mem <- readInputFileOrIntcode

  let (_, visit) = paint mem False
  print (Set.size visit)

  let (white, _) = paint mem True
  let (ys, xs) = unzip (Set.toList white)

  for_ [minimum ys .. maximum ys] $ \y ->
    putStrLn (map (\x -> if (y, x) `Set.member` white then '#' else ' ') [minimum xs .. maximum xs])
