import AdventOfCode (readInputFile)

import Data.Bits (shiftL, testBit)
import Data.List (foldl', sortOn)
import Data.Maybe (fromJust)

data Technique a = DealWith a | Cut a | Reverse deriving Show

simplifyPair :: Integral a => a -> Technique a -> Technique a -> [Technique a]
simplifyPair deckSize (DealWith inc1) (DealWith inc2) = [DealWith ((inc1 * inc2) `mod` deckSize)]
simplifyPair deckSize (Cut n) (DealWith inc) = [DealWith inc, Cut ((n * inc) `mod` deckSize)]
simplifyPair deckSize (Cut n1) (Cut n2) = [Cut ((n1 + n2) `mod` deckSize)]
simplifyPair deckSize Reverse (DealWith inc) = [DealWith (deckSize - inc), Cut inc]
simplifyPair _ Reverse (Cut n) = [Cut (-n), Reverse]
simplifyPair _ Reverse Reverse = []
simplifyPair _ a b = [a, b]

simplify :: Integral a => a -> [Technique a] -> [Technique a]
simplify deckSize = until ((<= 3) . length) (simplifyOdd . simplifyEven)
  where simplifyOdd (x:xs) = x : simplifyEven xs
        simplifyOdd [] = []
        simplifyEven (x1:x2:xs) = simplifyPair deckSize x1 x2 ++ simplifyEven xs
        simplifyEven xs = xs

parseTech :: Read a => String -> Technique a
parseTech s = case words s of
  ["deal", "with", "increment", n] -> DealWith (read n)
  ["cut", n] -> Cut (read n)
  ["deal", "into", "new", "stack"] -> Reverse
  w -> error ("invalid technique " ++ unwords w)

apply :: Integral a => a -> [Technique a] -> a -> a
apply deckSize techs cards = foldl' applyTech cards techs
  where applyTech pos (DealWith inc) = (pos * inc) `mod` deckSize
        applyTech pos (Cut n) = (pos - n) `mod` deckSize
        applyTech pos Reverse = deckSize - 1 - pos

unapply :: Integral a => a -> [Technique a] -> a -> a
unapply deckSize techs position = foldl' unapplyTech position (reverse techs)
  where unapplyTech pos (DealWith inc) = (pos * fromJust (modInv inc deckSize)) `mod` deckSize
        unapplyTech pos (Cut n) = (pos + n) `mod` deckSize
        unapplyTech pos Reverse = deckSize - 1 - pos

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- https://rosettacode.org/wiki/Modular_inverse#Haskell
-- Given a and m, return Just x such that ax = 1 mod m.
-- If there is no such x return Nothing.
modInv :: Integral a => a -> a -> Maybe a
modInv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Integral a => a -> a -> (a, a, a)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

main :: IO ()
main = do
  s <- readInputFile
  let techs = map parseTech (lines s) :: [Technique Integer]
      test = length techs < 40

  let deckSize1 = if test then 10 else 10007
      techs1 = simplify deckSize1 techs

  if test
    then do
      let applyDeck n = map snd (sortOn fst [(apply deckSize1 techs1 i, i) | i <- [0 .. n - 1]])
      putStrLn (unwords (map show (applyDeck deckSize1)))
    else do
      print (apply deckSize1 techs1 2019)

      let deckSize2 = 119315717514047
          iters = 101741582076661 :: Int
          techs2 = simplify deckSize2 techs
          bits = takeWhile ((<= iters) . shiftL 1 . fst) . enumerate . iterate (\ts -> simplify deckSize2 (ts ++ ts)) $ techs2
          relevantBits = filter (testBit iters . fst) bits
          final = simplify deckSize2 (concatMap snd relevantBits)
      print (unapply deckSize2 final 2020)
