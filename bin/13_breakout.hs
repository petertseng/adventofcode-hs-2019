import AdventOfCode.Intcode (Computer(output), computer, continueDefault, continueInput, readInputFileOrIntcode)

import Data.Set (Set)
import qualified Data.Set as Set

data Game = Game {
  paddleX :: Int
, ballX :: Int
, blocks :: Set (Int, Int)
, score :: Int
}

readOutput :: [Int] -> Game -> Game
readOutput (x:y:0:xs) g = readOutput xs (g { blocks = Set.delete (y, x) (blocks g) })
readOutput (x:y:2:xs) g = readOutput xs (g { blocks = Set.insert (y, x) (blocks g) })
readOutput (x:_:3:xs) g = readOutput xs (g { paddleX = x })
readOutput (x:_:4:xs) g = readOutput xs (g { ballX = x })
readOutput (-1:0:s:xs) g = readOutput xs (g { score = s })
readOutput (_:_:_:xs) g = readOutput xs g
readOutput [] g = g
readOutput o _ = error ("output not in threes: " ++ show o)

playGame :: (Computer, Game) -> (Computer, Game)
playGame (comp, game) = (comp', game')
  where input = case compare (ballX game) (paddleX game) of
          LT -> -1
          EQ -> 0
          GT -> 1
        comp' = continueInput input (comp { output = [] })
        game' = readOutput (output comp') game

main :: IO ()
main = do
  mem <- readInputFileOrIntcode

  let comp = continueDefault (computer (2 : drop 1 mem))
      newGame = Game { paddleX = undefined, ballX = undefined, blocks = Set.empty, score = 0 }
      game = readOutput (output comp) newGame

  print (Set.size (blocks game))

  let (_, winner) = until (Set.null . blocks . snd) playGame (comp, game)

  print (score winner)
