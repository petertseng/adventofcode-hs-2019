import AdventOfCode.Split (splitOn)
import AdventOfCode.Intcode (Computer, computer, continueDefault, continueInputs, output)

import Data.Foldable (for_, traverse_)
import Data.List (elemIndices, findIndex, group, intercalate, isPrefixOf, unfoldr)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int)

left :: Dir -> Dir
-- (-1, 0) -> (0, -1) -> (1, 0) -> (0, 1) -> (-1, 0)
left (Dir (dy, dx)) = Dir (-dx, dy)

right :: Dir -> Dir
right = left . left . left

step :: Pos -> Dir -> Pos
step (a, b) (Dir (c, d)) = (a + c, b + d)

scaffold :: String -> (Set Pos, Pos)
scaffold s = (scaffolds, robot)
  where grid = enumGrid (lines s)
        coordsWhere :: (Char -> Bool) -> [Pos]
        coordsWhere f = map fst (filter (f . snd) grid)
        scaffolds = Set.fromAscList (coordsWhere (== '#'))
        robot = head (coordsWhere (== '^'))

intersections :: Set Pos -> [Pos]
intersections poses = filter intersection (Set.toList poses)
  where intersection = all (`Set.member` poses) . neighbours
        neighbours (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

alignment :: Pos -> Int
alignment (y, x) = y * x

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [0..]

data Move = TurnRight | TurnLeft | Forward deriving Eq

maxLen :: Int
maxLen = 20

prog :: Pos -> Set Pos -> [String]
prog robot = compress . path robot

compress :: [Move] -> [String]
compress moves = assign "ABC" simple []
  where simple = intercalate "," (map repr (group moves))
        repr grp = case grp of
          [TurnRight] -> "R"
          [TurnLeft] -> "L"
          Forward:_ -> show (length grp)
          _ -> error "bad group"

assign :: String -> String -> [String] -> [String]
assign [] mainS _ | any (`notElem` "ABC,") mainS = []
assign [] mainS assigned = [unlines (mainS : reverse assigned)]
assign (x:xs) mainS assigned = case findIndex (`notElem` "ABC,") mainS of
  Nothing -> assign [] mainS (map (const "") (x:xs) ++ assigned)
  Just start -> concat [try len | len <- takeWhile (<= limit) commas]
    where try len =
            let f = take len mainAfter
                replaced = replace f x mainS
                tooManyFunc = count (`elem` "ABC") replaced * 2 - 1 > maxLen
            in if any (`elem` "ABC") f || tooManyFunc then []
            else assign xs replaced (f:assigned)
          commas = elemIndices ',' mainAfter
          limit = case findIndex (`elem` "ABC") mainAfter of
            Nothing -> maxLen
            Just i -> min i maxLen
          mainAfter = drop start mainS

path :: Pos -> Set Pos -> [Move]
path pos scaf = unfoldr (move scaf) (pos, Dir (-1, 0))

move :: Set Pos -> (Pos, Dir) -> Maybe (Move, (Pos, Dir))
move scaf (pos, dir) | fwd `Set.member` scaf = Just (Forward, (fwd, dir))
  where fwd = step pos dir
move scaf (pos, dir) =
  let ldir = left dir
      lpos = step pos ldir
      rdir = right dir
      rpos = step pos rdir
  in case (lpos `Set.member` scaf, rpos `Set.member` scaf) of
    (True, True) -> error "3-way intersection"
    (True, False) -> Just (TurnLeft, (pos, ldir))
    (False, True) -> Just (TurnRight, (pos, rdir))
    (False, False) -> Nothing

replace :: Eq a => [a] -> a -> [a] -> [a]
replace from to l | from `isPrefixOf` l = to : replace from to (drop (length from) l)
replace _ _ [] = []
replace from to (x:xs) = x : replace from to xs

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

dust :: Computer -> String -> [Int]
dust comp s = filter (> 127) (output comp')
  where comp' = continueInputs is comp
        is = map fromEnum (s ++ "\nn\n")

mapOrIntcode :: String -> (String, Maybe Computer)
mapOrIntcode s | ',' `elem` s = (map toEnum (output comp), Just comp)
  where comp = continueDefault (computer (2 : drop 1 mem))
        mem = map read (splitOn ',' s)
mapOrIntcode s = (s, Nothing)

main :: IO ()
main = do
  args <- getArgs
  s <- case args of
    [] -> readFile "/dev/stdin"
    a:_ | ',' `elem` a -> return a
    a:_ -> readFile a
  let (map', comp) = mapOrIntcode s
  let (scaffolds, robot) = scaffold map'
  print (sum (map alignment (intersections scaffolds)))

  let solns = prog robot scaffolds
  case comp of
    Nothing -> putStrLn ((if null solns then "im" else "") ++ "possible")
    Just comp' -> for_ solns (traverse_ print . dust comp')
