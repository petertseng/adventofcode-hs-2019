{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import Data.List (maximumBy, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set

type AsteroidField = (Int, Int, Set (Int, Int))
type Pos = (Int, Int)
newtype Dir = Dir (Int, Int) deriving (Eq)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

maximum' :: Ord a => [a] -> Maybe a
maximum' [] = Nothing
maximum' l = Just (maximum l)

asteroidPos :: (Int, Char) -> Maybe Int
asteroidPos (n, '#') = Just n
asteroidPos _ = Nothing

directions :: Int -> Int -> [Dir]
directions h w = [Dir (dy, dx) | dy <- [-h .. h]
                               , dx <- [-w .. w]
                               , gcd dy dx == 1]

step :: Pos -> Dir -> Pos
step (a, b) (Dir (c, d)) = (a + c, b + d)

asteroidInDir :: Pos -> AsteroidField -> Dir -> Maybe Pos
asteroidInDir asteroid (h, w, asts) dir = travel (step asteroid dir)
  where travel (y, x) | (y, x) `Set.member` asts = Just (y, x)
        travel (y, x) | y < 0 || x < 0 || y >= h || x >= w = Nothing
        travel place = travel (step place dir)

detect :: AsteroidField -> Pos -> Pos -> Bool
detect _ a1 a2 | a1 == a2 = False
detect field a1@(y1, x1) a2@(y2, x2) = asteroidInDir a1 field (Dir (dy', dx')) == Just a2
  where dy = y2 - y1
        dx = x2 - x1
        -- maybe a little too clever?
        -- if either is zero, use abs of the other
        -- if neither are, gcd
        g = if dy * dx == 0 then abs (dx + dy) else gcd dy dx
        dy' = dy `div` g
        dx' = dx `div` g

destroy :: Pos -> AsteroidField -> [Dir] -> [Pos]
destroy station (height, width, asteroids) dirs = destroy' dirs (Set.delete station asteroids)
  where destroy' [] _ = []
        destroy' _ as | Set.null as = []
        destroy' (dir:rest) as = case asteroidInDir station (height, width, as) dir of
          Nothing -> destroy' rest as
          Just destroyed -> destroyed : destroy' rest (Set.delete destroyed as)

main :: IO ()
main = do
  s <- readInputFile
  let asteroids = concatMap (\(y, row) -> map (y,) (mapMaybe asteroidPos (enumerate row))) (enumerate (lines s))
      asteroidSet = Set.fromAscList asteroids
      height = length (lines s)
      width = fromMaybe 0 (maximum' (map length (lines s)))
      field = (height, width, asteroidSet)

  let counts = map (\a -> (a, count (detect field a) asteroids)) asteroids
      (station, asteroidsSeen) = maximumBy (comparing snd) counts
  print asteroidsSeen

  if Set.size asteroidSet > 200
    then do
      let dirs = directions height width
      -- atan2: -pi at (-0.0, -1.0) and +pi at (0.0, -1.0)
      -- Since we want to start at (-1, 0), we need to give atan2 reflected axes.
      let clockwise = sortOn (\(Dir (dy, dx)) -> -atan2 (fromIntegral dx) (fromIntegral dy) :: Double) dirs
      let (y, x) = destroy station field (cycle clockwise) !! 199
      print (x * 100 + y)
    else
      putStrLn ("bad " ++ show (Set.size asteroidSet))
