module AdventOfCode.Intcode (
  Computer(..),
  Opts(..),
  computer,
  continue,
  continueDefault,
  continueInput,
  continueInputs,
  defaults,
  memRead,
  memWrite,
  readInputFileOrIntcode,
  writes,
) where

import Data.Array.IArray ((!), listArray)
import Data.Array.Unboxed (UArray)
import Data.Maybe (fromMaybe, maybeToList)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import System.Environment (getArgs)

data Computer = Computer {
    pos :: Int
  , romem :: UArray Int Int
  , rwmem :: IntMap Int
  , unusedInputs :: [Int]
  , output :: [Int]
  , halt :: Bool
  , block :: Bool
  }

data Opts = Opts {
    input :: Maybe Int
  , inputs :: [Int]
  }

defaults :: Opts
defaults = Opts {
    input = Nothing
  , inputs = []
  }

computer :: [Int] -> Computer
computer mem = Computer {
    pos = 0
  , romem = listArray (0, length mem - 1) mem
  , rwmem = IntMap.empty
  , unusedInputs = []
  , output = []
  , halt = False
  , block = False
  }

data Op =
    Halt
  | Binop (Int -> Int -> Int)
  | Input
  | Output
  | Jump (Int -> Bool)

intToOp :: Int -> Op
intToOp 99 = Halt
intToOp 1 = Binop (+)
intToOp 2 = Binop (*)
intToOp 3 = Input
intToOp 4 = Output
intToOp 5 = Jump (/= 0)
intToOp 6 = Jump (== 0)
intToOp 7 = Binop (\a b -> if a < b then 1 else 0)
intToOp 8 = Binop (\a b -> if a == b then 1 else 0)
intToOp code = error ("unknown opcode " ++ show code)

numParams :: Op -> Int
numParams Halt = 0
numParams (Binop _) = 3
numParams Input = 1
numParams Output = 1
numParams (Jump _) = 2

numOutputs :: Op -> Int
numOutputs (Binop _) = 1
numOutputs Input = 1
numOutputs _ = 0

data Mode =
    Position
  | Immediate

intToModes :: Int -> [Mode]
intToModes 0 = repeat Position
intToModes n | n `mod` 10 == 1 = Immediate : intToModes (n `div` 10)
intToModes n | n `mod` 10 == 0 = Position : intToModes (n `div` 10)
intToModes n = error ("bad mode " ++ show n)

continue :: Opts -> Computer -> Computer
continue opts = until (\c -> halt c || block c) step . addInputs
  where addInputs comp = comp { unusedInputs = inputs' comp, block = False }
        inputs' comp = unusedInputs comp ++ maybeToList (input opts) ++ inputs opts

continueDefault :: Computer -> Computer
continueDefault = continue defaults

continueInput :: Int -> Computer -> Computer
continueInput inval = continue (defaults { input = Just inval })

continueInputs :: [Int] -> Computer -> Computer
continueInputs invals = continue (defaults { inputs = invals })

memRead :: Int -> Computer -> Int
memRead addr Computer { romem = ro, rwmem = rw } =
  fromMaybe (ro ! addr) (IntMap.lookup addr rw)

memWrite :: Int -> Int -> Computer -> Computer
memWrite addr val comp = comp { rwmem = IntMap.insert addr val (rwmem comp) }

writes :: Computer -> [(IntMap.Key, Int)]
writes = IntMap.toList . rwmem

step :: Computer -> Computer
step comp = let opcode = memRead (pos comp) comp in
  step' (intToOp (opcode `mod` 100)) (intToModes (opcode `div` 100)) comp

step' :: Op -> [Mode] -> Computer -> Computer
step' op modes comp =
  let values = [memRead (pos comp + n) comp | n <- [1 .. numParams op]]
      numInputs = numParams op - numOutputs op

      resolv :: Int -> Mode -> Int
      resolv ps Position = memRead ps comp
      resolv imm Immediate = imm

      resolved = zipWith resolv (take numInputs values) modes
      pos' = pos comp + 1 + numParams op
      computer' = comp { pos = pos' }
  in case op of
  Halt -> computer' { halt = True }
  Binop f -> memWrite (last values) (foldl1 f resolved) computer'
  Input -> case unusedInputs comp of
    [] -> comp { block = True } -- keep same pos, so comp not computer'
    x:xs -> memWrite (last values) x computer' { unusedInputs = xs }
  Output -> computer' { output = output computer' ++ resolved }
  Jump p -> case resolved of
    [test, posIfTrue] -> computer' { pos = if p test then posIfTrue else pos' }
    _ -> error "improper resolve of jump"

readInputFileOrIntcode :: IO [Int]
readInputFileOrIntcode = do
  args <- getArgs
  case args of
    [] -> fmap (map read . splitOn ',') (readFile "/dev/stdin")
    a:_ | ',' `elem` a -> return (map read (splitOn ',' a))
    a:_ -> fmap (map read . splitOn ',') (readFile a)

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
        where (w, s'') = break (== c) s'
