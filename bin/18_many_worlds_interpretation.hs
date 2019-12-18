{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Search (astar, bfs)

import Data.Array.Unboxed ((!), UArray, array, bounds)
import Data.Bits ((.|.), (.&.), complement, countLeadingZeros, finiteBitSize, setBit, shiftL, shiftR)
import Data.Char (isLower, isUpper)
import Data.Either (rights)
import Data.Function (on)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

type Pos = (Int, Int)
type Vault = UArray Pos Char
data KeyPath = KeyPath { kIdx :: Int, kPos :: Pos, kDist :: Int, kKeys :: Int, kDoors :: Int } deriving Show

instance Eq KeyPath where
  (==) = (==) `on` kPos

instance Ord KeyPath where
  compare = compare `on` kPos

keyTime :: Map Int [KeyPath] -> Int -> [Int] -> Maybe Int
keyTime keysFrom numKeys robots = astar keyNeigh farthestKeys haveAllKeys start
  where haveAllKeys botsAndKeys = botsAndKeys .&. allKeys == allKeys
        allKeys = shiftL 1 numKeys - 1
        bitsPerRobot = bits (Map.size keysFrom)
        robotMask = shiftL 1 bitsPerRobot - 1
        robotBase = [bitsPerRobot * i + numKeys | i <- [0 .. length robots - 1]]
        start = sum (zipWith shiftL robots robotBase)
        bits x = finiteBitSize x - countLeadingZeros x

        keyNeigh :: Int -> [(Int, Int)]
        keyNeigh botsAndKeys = concatMap moveBot botsAndBases
          where botsAndBases = [(base, shiftR botsAndKeys base .&. robotMask) | base <- robotBase]
                keys = botsAndKeys .&. allKeys
                moveBot (base, bot) = mapMaybe (moveToKey base) (Map.findWithDefault [] bot keysFrom)
                moveToKey _ key | kKeys key .|. keys == keys = Nothing
                moveToKey _ key | kDoors key .|. keys /= keys = Nothing
                moveToKey base key = Just (kDist key, botsWithout base .|. shiftL (kIdx key) base .|. kKeys key)
                botsWithout base = botsAndKeys .&. complement (shiftL robotMask base)

        farthestKeys :: Int -> Int
        farthestKeys botsAndKeys = sum [farthestKey (shiftR botsAndKeys base .&. robotMask) | base <- robotBase]
          where maximum' [] = 0
                maximum' xs = maximum xs
                keys = botsAndKeys .&. allKeys
                farthestKey robot = maximum' (map kDist (newKeys robot))
                newKeys robot = filter (\kp -> kKeys kp .|. keys /= keys) (Map.findWithDefault [] robot keysFrom)

keyToKey :: Vault -> [Pos] -> Map (Int, Int) KeyPath
keyToKey vault poses = Map.fromList . concatMap (\srcp -> map (\kp -> ((idx Map.! srcp, kIdx kp), kp)) (keyPathsFrom vault idx srcp)) $ poses
  where idx = Map.fromList (zip poses [0..])

keyPathsFrom :: Vault -> Map Pos Int -> Pos -> [KeyPath]
keyPathsFrom vault idx pos = map finalise . rights $ bfs (vaultNeigh vault pos) (vaultNewKey vault pos) k
  where k = KeyPath { kIdx = idx Map.! pos, kPos = pos, kDist = 0, kKeys = 0, kDoors = 0 }
        finalise (dist, keyPath) = keyPath { kIdx = idx Map.! kPos keyPath, kDist = dist }

vaultNeigh :: Vault -> Pos -> KeyPath -> [KeyPath]
vaultNeigh vault start kp | vaultNewKey vault start kp = []
vaultNeigh vault _ kp@KeyPath { kPos = (y, x), kKeys = keys, kDoors = doors } = mapMaybe move moves
  where moves = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
        move pos = case vault ! pos of
          '#' -> Nothing
          c | isLower c -> Just kp { kPos = pos, kKeys = setBit keys (fromEnum c - fromEnum 'a') }
          c | isUpper c -> Just kp { kPos = pos, kDoors = setBit doors (fromEnum c - fromEnum 'A') }
          '.' -> Just kp { kPos = pos }
          '@' -> Just kp { kPos = pos }
          c -> error ("bad " ++ show c)

vaultNewKey :: Vault -> Pos -> KeyPath -> Bool
vaultNewKey vault start KeyPath { kPos = pos } = pos /= start && isLower (vault ! pos)

allPairs :: Ord a => Map (a, a) KeyPath -> Map a [KeyPath]
allPairs k2k = flatten $ foldl' better k2k [(i, j, k) | k <- poses, i <- poses, j <- poses]
  where poses = nubConsec . map fst . Map.keys $ k2k
        better m (i, j, k) | i == j || i == k || j == k = m
        better m (i, j, k) = case (Map.lookup (i, k) m, Map.lookup (k, j) m) of
          (Just ik, Just kj) ->
            let newDist = kDist ik + kDist kj
                ijBetter = maybe True (\kp -> kDist kp > newDist) (Map.lookup (i, j) m)
                newKp = kj { kDist = newDist, kKeys = kKeys ik .|. kKeys kj, kDoors = kDoors ik .|. kDoors kj }
            in if ijBetter then Map.insert (i, j) newKp m else m
          _ -> m

nubConsec :: Eq a => [a] -> [a]
nubConsec (x:y:xs) | x == y = x : nubConsec xs
nubConsec (x:xs) = x : nubConsec xs
nubConsec [] = []

flatten :: Ord a => Map (a, a) KeyPath -> Map a [KeyPath]
flatten = Map.fromListWith (++) . map (\((a, _), kp) -> (a, [kp])) . Map.toList

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [1..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [1..]

main :: IO ()
main = do
  s <- readInputFile
  let grid = enumGrid (lines s)
      height = maximum (map (fst . fst) grid)
      width = maximum (map (snd . fst) grid)
      vault = array ((1, 1), (height, width)) grid

      robots = map fst (filter ((== '@') . snd) grid)
      keys = map fst (filter (isLower . snd) grid)
      numKeys = length keys

      robot = head robots
      diagonal (y, x) = [(y - 1, x - 1), (y - 1, x + 1), (y + 1, x - 1), (y + 1, x + 1)]
      orthogonal (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
      surrounding = diagonal robot ++ orthogonal robot
      canPart2 = length robots == 1 && all (\pos -> vault ! pos == '.') surrounding
      printMaybe = putStrLn . maybe "impossible" show

  if canPart2
    then do
      let robots2 = diagonal robot
          vault2 = array (bounds vault) (grid ++ map (, '#') (concatMap orthogonal robots))
          keysFrom2 = keyToKey vault2 (robot : robots2 ++ keys)
          kp i1 i2 dist = ((i1, i2), KeyPath { kIdx = i2, kPos = undefined, kDist = dist, kKeys = 0, kDoors = 0 })

          centre = concatMap (pair 2 0) [1..4]
          pair dist i1 i2 = [kp i1 i2 dist, kp i2 i1 dist]

          robots2Idx = zip robots2 [1..]
          cornerToCorner = mapMaybe (uncurry c2c) [(pos1, pos2) | pos1 <- robots2Idx, pos2 <- robots2Idx]
          c2c ((y1, x1), _) ((y2, x2), _) | (y1 == y2) == (x1 == x2) = Nothing
          c2c (_, i1) (_, i2) = Just (kp i1 i2 2)

          backToCorner = mapMaybe (uncurry b2c) (Map.toList keysFrom2)
          b2c (i1, _) _ | 0 >= i1 || i1 > 4 = Nothing
          b2c (i1, i2) k = Just ((i2, i1), k { kPos = undefined, kIdx = i1 })

          keysFrom = Map.unionWith (error "dup key") keysFrom2 (Map.fromList (centre ++ cornerToCorner ++ backToCorner))

      printMaybe (keyTime (allPairs keysFrom) numKeys [0])
      printMaybe (keyTime (allPairs keysFrom2) numKeys [1 .. length robots2])
    else do
      let keysFrom = keyToKey vault (robots ++ keys)
      printMaybe (keyTime (allPairs keysFrom) numKeys [0 .. length robots - 1])
