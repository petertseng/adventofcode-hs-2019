# Advent of Code

[![Build Status](https://travis-ci.org/petertseng/adventofcode-hs-2019.svg?branch=master)](https://travis-ci.org/petertseng/adventofcode-hs-2019)

These are my solutions to http://adventofcode.com

All solutions are written in Haskell.

I didn't feel like writing day 25.

## Input

In general, all solutions can be invoked in both of the following ways:

* Without command-line arguments, takes input on standard input.
* With 1+ command-line arguments, reads input from the first, which must be the path to an input file.
  Arguments beyond the first are ignored.

Some may additionally support other ways:

* All intcode days: May pass the intcode in ARGV as a single argument separated by commas.
* Day 04 (Password): May pass min and max in ARGV (as two args, or as one arg joined by a hyphen).

## Closing thoughts

Non-strict evaluation comes in handy, especially when different days might share a common approach but impose different termination conditions.
For example, some days want to BFS all goals, but some just want to BFS a single goal.
In other languages, that is most conveniently achieved by adding a parameter that controls how many goals the BFS attempts to find.
In Haskell, the BFS simply attempts to find them all, and if a day only wants one goal, it only asks for one.
Thus, the others don't get computed.

I can apparently be extraordinarily uncreative with names - use `git grep "''"` to see how many times I use `''`.
