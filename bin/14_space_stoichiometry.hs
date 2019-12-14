{-# LANGUAGE NumericUnderscores #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Control.Arrow ((***), second)
import Data.Char (isDigit)
import Data.List (dropWhileEnd, mapAccumL, partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)

type Recipes = Map String (Int, [(String, Int)])

readRecipe :: String -> (String, (Int, [(String, Int)]))
readRecipe s = (resultName, (resultAmount, ingredients'))
  where (ingredients, result) = (dropWhileEnd (== ' ') *** dropWhile (not . isDigit)) (splitOnOne '=' s)
        (resultName, resultAmount) = nameAndAmount result
        ingredients' = map (nameAndAmount . dropWhile (== ' ')) (splitOn ',' ingredients)

nameAndAmount :: String -> (String, Int)
nameAndAmount = second read . swap . splitOnOne ' '

oreToMakeFuel :: Recipes -> Int -> Int
oreToMakeFuel r fuel = oreToMake r Map.empty [("FUEL", fuel)]

oreToMake :: Recipes -> Map String Int -> [(String, Int)] -> Int
oreToMake _ _ [] = 0
oreToMake recipes leftovers things = sum (map snd ores) + oreToMake recipes leftovers' (combine notOres)
  where (leftovers', things') = mapAccumL (make recipes) leftovers things
        (ores, notOres) = partition ((== "ORE") . fst) (concat things')

combine :: (Num b, Ord a) => [(a, b)] -> [(a, b)]
combine = Map.toList . Map.fromListWith (+)

make :: Recipes -> Map String Int -> (String, Int) -> (Map String Int, [(String, Int)])
make recipes leftovers (thing, amountNeeded) = (leftovers', inputs')
  where (produced, inputs) = recipes Map.! thing
        useLeftover = min amountNeeded (Map.findWithDefault 0 thing leftovers)
        amountNeeded' = amountNeeded - useLeftover
        times = amountNeeded' `ceilDiv` produced
        excess = times * produced - amountNeeded'
        leftovers' = Map.insertWith (+) thing (excess - useLeftover) leftovers
        inputs' = if times == 0 then [] else map (second (* times)) inputs

ceilDiv :: Int -> Int -> Int
a `ceilDiv` b = -(a `div` (-b))

bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch f low high
  | low > high = high
  | f mid = bsearch f low (mid - 1)
  | otherwise = bsearch f (mid + 1) high
  where mid = low + ((high - low) `div` 2)

main :: IO ()
main = do
  s <- readInputFile
  let recipes = Map.fromList (map readRecipe (lines s))

  let ore1 = oreToMakeFuel recipes 1
  print ore1

  let trillion = 1_000_000_000_000
      lowerBound = trillion `div` ore1
      tooMuch x = oreToMakeFuel recipes x > trillion
      upperBound = until tooMuch (* 2) lowerBound

  print (bsearch tooMuch lowerBound upperBound)
