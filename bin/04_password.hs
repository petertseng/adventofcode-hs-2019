import AdventOfCode.Split (splitOnOne)

import Data.List (dropWhileEnd, elemIndex, group)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.FilePath (takeFileName)
import Prelude hiding (max, min)

increasingBetween :: String -> String -> [String]
increasingBetween min max = takeWhile (<= max) (iterate (step len) min')
  where len = length max
        goodMin = fromMaybe len (elemIndex True (zipWith (>) min (drop 1 min)))
        min' = take goodMin min ++ replicate (len - goodMin) (min !! goodMin)

step :: Int -> String -> String
step len x = init notNine ++ replicate (len - length notNine + 1) (succ (last notNine))
  where notNine = dropWhileEnd (== '9') x

anyGroup :: Eq a => ([a] -> Bool) -> [a] -> Bool
anyGroup f = any f . group

ok1 :: [a] -> Bool
ok1 (_:_:_) = True
ok1 _ = False
-- not sure whether it's faster than (>= 2) . length

ok2 :: [a] -> Bool
ok2 [_, _] = True
ok2 _ = False
-- not sure whether it's faster than (== 2) . length

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  args <- getArgs
  let splitDash = splitOnOne '-'

  (min, max) <- case args of
    [] -> fmap splitDash (readFile "/dev/stdin")
    [f] | '-' `elem` takeFileName f -> return (splitDash f)
    [f] -> fmap splitDash (readFile f)
    a:b:_ -> return (a, b)

  let candidates = increasingBetween min max
  print (count (anyGroup ok1) candidates)
  print (count (anyGroup ok2) candidates)
