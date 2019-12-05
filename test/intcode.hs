module Main (main) where

import AdventOfCode.Intcode (computer, continueDefault, continueInput, halt, output, writes)
import System.Exit (ExitCode(..), exitWith)
import Test.HUnit (Counts(..), Test(..), assertEqual, runTestTT)

addMulTable :: [([Int], [(Int, Int)])]
addMulTable = [
    ([1,9,10,3,2,3,11,0,99,30,40,50], [(0, 3500), (3, 70)])
  , ([1,0,0,0,99], [(0, 2)])
  , ([2,3,0,3,99], [(3, 6)])
  , ([2,4,4,5,99,0], [(5, 9801)])
  , ([1,1,1,4,99,5,6,0,99], [(0, 30), (4, 2)])
  ]

addMulTests :: [Test]
addMulTests = map toTest addMulTable
  where toTest (mem, expectWrites) = TestCase (assertEqual (show mem) expectWrites (run mem))
        run = writes . continueDefault . computer

ioTest :: Test
ioTest = TestCase (assertEqual (show mem) [77] (run mem))
  where run = output . continueInput 77 . computer
        mem = [3,0,4,0,99]

paramModeTable :: [[Int]]
paramModeTable = [
    [1002,4,3,4,33]
  , [1101,100,-1,4,0]
  ]

paramModeTests :: [Test]
paramModeTests = map toTest paramModeTable
  where toTest mem = TestCase (assertEqual (show mem) True (halts mem))
        halts = halt . continueDefault . computer

cmpJmpTable :: [([Int], [(Int, Int)])]
cmpJmpTable = [
    ([3,9,8,9,10,9,4,9,99,-1,8], [(7, 0), (8, 1), (9, 0)])
  , ([3,9,7,9,10,9,4,9,99,-1,8], [(7, 1), (8, 0), (9, 0)])
  , ([3,3,1108,-1,8,3,4,3,99], [(7, 0), (8, 1), (9, 0)])
  , ([3,3,1107,-1,8,3,4,3,99], [(7, 1), (8, 0), (9, 0)])
  , ([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], [(-1, 1), (0, 0), (1, 1)])
  , ([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], [(-1, 1), (0, 0), (1, 1)])
  , ([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], [(6, 999), (7, 999), (8, 1000), (9, 1001), (10, 1001)])
  ]

cmpJmpTests :: [Test]
cmpJmpTests = concatMap toTests cmpJmpTable
  where toTests (mem, pairs) = map (toTest mem) pairs
        toTest mem (inval, out) = TestCase (assertEqual (show mem ++ " input " ++ show inval) [out] (run inval mem))
        run inval = output . continueInput inval . computer

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

main :: IO ()
main = exitProperly $ runTestTT tests
  where tests = TestList [
            TestLabel "addMul" (TestList addMulTests)
          , TestLabel "I/O" ioTest
          , TestLabel "param mode" (TestList paramModeTests)
          , TestLabel "cmpJmp" (TestList cmpJmpTests)
          ]
