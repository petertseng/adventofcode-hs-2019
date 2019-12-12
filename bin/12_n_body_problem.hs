{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Data.List (dropWhileEnd, find, foldl', transpose)
import Data.Maybe (fromJust)

type Pos = Int
type Vel = Int

readMoon :: String -> [(Pos, Vel)]
readMoon = map ((, 0) . read . snd . splitOnOne '=') . splitOn ',' . dropWhileEnd (== '>')

step :: [(Pos, Vel)] -> [(Pos, Vel)]
step moons = map (stepPos . stepVel) moons
  where stepVel (p, v) = (p, dv p + v)
        poses = map fst moons
        dv p = sum (map (signum . flip (-) p) poses)
        stepPos (p, v) = (p + v, v)

energy :: [[(Pos, Vel)]] -> Int
energy = sum . map energy'
  where energy' moon = let (poses, vels) = unzip moon in sumAbs poses * sumAbs vels
        sumAbs = sum . map abs


period :: [(Pos, Vel)] -> Int
period moons = if v0moons == moons then t else t * 2
  where (t, v0moons) = fromJust (find vels0 steps)
        vels0 = all ((== 0) . snd) . snd
        steps = zip [1..] (drop 1 (iterate step moons))

main :: IO ()
main = do
  s <- readInputFile
  let coords = transpose (map readMoon (lines s))
      after1k = transpose (map (\c -> iterate step c !! 1000) coords)
  print (energy after1k)

  let periods = map period coords
  print (foldl' lcm 1 periods)
