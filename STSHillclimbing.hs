module STSHillclimbing
( findSTS
, findSTSUntilSuccess
, makeSTS
) where

import Control.Monad.State
import Data.List
import System.Random

type NumPoints = Int
type Point = Int
type Triple = (Int, Int, Int)
type Pair = (Int, Int)
type PSTS = [Triple]
type IterationsLeft = Int
type RNG = StdGen

randomNext :: (RandomGen g) => State g Int
randomNext = state $ next

-- Pick a single element of a list.
-- Used to pick a point x with uncovered pairs to consider, and then again by pickTwoFromList to select two
-- points y and z such that (x,y) and (y,z) are not covered.
--pickFromList :: [a] -> State RNG a
--pickFromList lst = do
--   gen <- get
--   let (idx, gen') = next gen
--   put gen'
--   return (lst!!(idx `mod` (length lst)))
pickFromList :: [a] -> State RNG a
pickFromList lst = do
   idx <- randomNext
   return (lst!!(idx `mod` (length lst)))


-- Remove all occurrences of an element from a list.
-- Used by pickTwoFromList.
remove :: Eq a => a -> [a] -> [a]
remove x = filter (x /=)

-- Pick a pair of distinct elements from a list.
-- We need this so that if we have, for a point x, a set of points S so that y in S implies the pair (x,y) is not
-- yet covered by a triple, we can select two uncovered pairs with x, namely (x,y) and (x,z).
-- We do this on a list of pairs not appearing with x to get a y and z so that xy and xz are not covered by a triple.
-- We assume that the list has two distinct elements: otherwise an exception will be thrown.
pickTwoFromList :: Eq a => [a] -> State RNG (a,a)
pickTwoFromList lst = do
    y <- pickFromList lst
    z <- pickFromList (remove y lst)
    return (y,z)

-- Determine if a specified pair is covered by a triple.
-- This is used when we select uncovered pairs (x,y) and (x,z) to filter out any triple containing (y,z).
pairIsInTriple :: Pair -> Triple -> Bool
pairIsInTriple (p1, p2) (t1, t2, t3) =
  let lst = [t1, t2, t3]
  in p1 `elem` lst && p2 `elem` lst

-- Drop any triples covering a pair (y,z). This is so we can add the triple (x,y,z) to the STS.
dropTripleCoveringPair :: PSTS -> Pair -> PSTS
dropTripleCoveringPair [] _ = []
dropTripleCoveringPair psts p = filter (not . pairIsInTriple p) psts

-- Sort a triple in ascending order.
sortTriple :: Triple -> Triple
sortTriple (a, b, c) =
  let p1 = min (min a b) c
      p3 = max (max a b) c
      p2 = [x | x <- [a,b,c], x /= p1, x/= p3]!!0
      in (p1, p2, p3)

-- Determine whether or not a point is in a triple.
tripleContains :: Point -> Triple -> Bool
tripleContains x (a, b, c) = x == a || x == b || x == c

-- In a triple (x,y,z), return [y,z].
-- If the triple does not contain x, it will return the whole triple, but we don't worry about this as it won't happen.
otherElements :: Point -> Triple -> [Point]
otherElements x (a, b, c) = filter (x /=) [a,b,c]

-- We need to know what pairs are uncovered.
-- We represent this information in the form of a list of lists, where
-- index x contains the list of points P such that for all y in P, (x,y) is not yet covered.

-- Return a list of the point appearing in triples with a specified point.
pointsAppearingWith :: Point -> PSTS -> [Point]
pointsAppearingWith x = concat . map (otherElements x) . filter (tripleContains x)

-- [0..n] \ [x]
rangeMinusOne :: Int -> Int -> [Int]
rangeMinusOne n x = [0..(x-1)] ++ [(x+1)..(n-1)]

-- Return a list of the points not appearing in triples with a specified point x.
-- This allows us to know what pairs with x are still uncovered.
elementsNotAppearingWith :: NumPoints -> Point -> PSTS -> [Point]
elementsNotAppearingWith n x = (\\) (rangeMinusOne n x) . pointsAppearingWith x

-- Finally, return the list of lists as described above.
uncoveredPairs :: NumPoints -> PSTS -> [[Point]]
uncoveredPairs n ts = map (\y -> elementsNotAppearingWith n y ts) [0..(n-1)]

-- Determine if a list is nonempty.
nonEmpty :: [a] -> Bool
nonEmpty = not . null

-- Lastly, we don't want any entries for any point that has less than two uncovered pairs.
-- Thus, we zip with index and filter out any points with fewer than two uncovered pairs.
-- TODO: there has to be a cleaner way to write this as pure function composition and $.
-- TODO: I am still a Haskell noob, clearly.
uncoveredPairsWithPoint :: NumPoints -> PSTS -> [(Point, [Point])]
uncoveredPairsWithPoint n = (filter (nonEmpty . snd)) . (zip [0..]) . uncoveredPairs n

-- Finds a new triple, adds it, and drops any triples violated by its addition.
addTriple :: PSTS -> [(Point, [Point])] -> State RNG PSTS
addTriple psts [] = do return psts
addTriple psts uncovered = do
  (x, missing) <- pickFromList uncovered
  (p@(y,z))    <- pickTwoFromList missing
  return ((sortTriple (x,y,z)) : (dropTripleCoveringPair psts p))

-- An STS(n) exists iff n = 1,3 (mod 6).
stsExistenceCondition :: NumPoints -> Bool
stsExistenceCondition n =
  let nMod6 = n `mod` 6
  in nMod6 == 1 || nMod6 == 3

----------------------------------------------------------------------------------------------

-- Now we actually implement the hillclimbing algorithm.

-- This takes a PSTS, finds two uncovered pairs xy and xz, and adds triple xyz to the PSTS.
-- If there was already a triple wyz, we drop that.
-- Continue until all pairs are covered or we run out of iterations.
-- Parameters:
-- 1. Number of iterations left to perform.
-- 2. The number of points.
-- 3. The PSTS so far.
-- Returns: Just PSTS if completed, and otherwise, Nothing.
hillclimb :: IterationsLeft -> NumPoints -> PSTS -> State RNG (Maybe PSTS)
hillclimb 0 _ _ = do return Nothing
hillclimb i n psts = 
     let uncovered = uncoveredPairsWithPoint n psts
     in if nonEmpty uncovered then do
           psts' <- addTriple psts uncovered
           hillclimb (i-1) n psts'
        else do return (Just psts)

-- Attempt to find an STS with a limited number of iterations.
-- If n != 1,3 (mod 6), terminates with an error.
findSTS :: IterationsLeft -> NumPoints -> State RNG (Maybe PSTS)
findSTS i n =
    if stsExistenceCondition n then
      hillclimb i n []
    else
      error "STS only exist for n = 1,3 (mod 6)"


-- Attempt to find an STS with an unlimited number of iterations.
-- If n != 1,3 (mod 6), terminates with an error.
findSTSUntilSuccess :: NumPoints -> State RNG PSTS
findSTSUntilSuccess n = do
  sol <- (findSTS (-1) n)
  case sol of
    Just psts -> return psts
    Nothing   -> return [] -- This should never happen.

makeSTS :: NumPoints -> IO PSTS
makeSTS n = do
  rng <- getStdGen
  return (evalState (findSTSUntilSuccess n) rng)