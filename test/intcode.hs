module Main (main) where

import AdventOfCode.Intcode (computer, continueDefault, writes)
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

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

main :: IO ()
main = exitProperly $ runTestTT tests
  where tests = TestList [
            TestLabel "addMul" (TestList addMulTests)
          ]
