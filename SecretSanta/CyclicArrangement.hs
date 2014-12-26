{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SecretSanta.CyclicArrangement
  ( CyclicArrangement
  , selectCyclicArrangement
  ) where

import Control.Applicative ((<$>),(<*>),(<|>))
import Control.Arrow (second)
import Control.Monad.State
import Data.List     (delete,find,minimumBy)
import Data.Map      ((!),assocs,empty,fromList,keys)
import Data.Foldable (foldl')
import Data.Maybe    (mapMaybe)
import Data.Ord      (comparing)
import Data.Tree
import SecretSanta.Types
import System.Random
import System.Random.Shuffle

-- | Represents a valid secret santa arrangement
-- | which is also a Hamiltonian Cycle
instance Constrainable CyclicArrangement where
  feasibleArrangements = feasibleCyclicArrangements
  toArrangement        = cycleToMap
  selectArrangement    = selectCyclicArrangement

-- | Converts a CyclicArrangement to a normal Arrangement
cycleToMap :: CyclicArrangement -> Arrangement
cycleToMap     [] = fromList []
cycleToMap (x:xs) = fromList $ cycleToMap' (x:xs)
  where
    cycleToMap'       [] = []
    cycleToMap'      [y] = [(y,x)]
    cycleToMap' (y:z:zs) =  (y,z) : cycleToMap' (z:zs)

-- | List of all hamiltonian cycles satisfying the constraints  
feasibleCyclicArrangements :: ConstraintMap -> [CyclicArrangement]
feasibleCyclicArrangements xs = cycles len $ mapToTree xs
  where len = foldl' (\x _ -> x+1) 0 xs

-- | Builds an intermidate Tree data structure used for cycle search
mapToTree :: ConstraintMap -> Tree Name
mapToTree = constructNode
        <$> assocs
        <*> (minimumBy (comparing (length . snd)) . assocs)

-- | Recursively builds Tree from associating list
constructNode :: Eq a => [(a,[a])] -> (a,[a]) -> Tree a
constructNode ys (x,xs)
  -- if the "MapList" is empty, we cannot recurse
  | null ys   = Node x []

  -- otherwise we create the node and recurse for it's children
  | otherwise = Node x $ fmap (constructNode ys') xs'
  where
    -- remove the current node from the "MapList"
    -- remove it's reference from the value list of all keys
    ys' = fmap (second (delete x)) $ delete (x,xs) ys

    -- get the node data to expand
    -- ignore nodes with
    xs' = mapMaybe (flip find ys' . (\y -> (==y).fst)) xs

-- | Augmented Depth-Limited Search on tree structure
-- | to find hamiltonian cycles satisfying the constraints
cycles :: Int -> Tree Name -> [CyclicArrangement]
cycles 1 tree = [[rootLabel tree]]
cycles n tree
  | (null . subForest) tree = []
  | otherwise = concatMap
              ( fmap (rootLabel tree :)
              . filter (not.null)
              . cycles (n-1)
              ) $ subForest tree

-- | Augmented depth-limited search
-- | over a randomization of the hamiltonian cycle search space.
-- | Returns the first valid hamiltonian cycle found during the random search
-- | /Ω(n)/ & /O(n!)/
-- | Best Case Complexity:
-- |   Linear time to generate a valid solution in the first few attempts
-- | Worst Case Complexity:
-- |   Factorial time to generate all possible solutions
-- |   and deterimine none satisfy constraints

selectCyclicArrangement :: ConstraintMap -> IO (Maybe CyclicArrangement)
selectCyclicArrangement originalConstraints =
  newStdGen >>= return . selectCyclicArrangement' originalConstraints 0 root
  where
    -- The height of the expanded tree iff it
    -- contains a path representing a hamiltonian cycle
    height = pred . length $ keys originalConstraints

    -- Without loss of generality, we can arbitrarily fix the root node
    -- We choose for complexity sake, to select the key with
    -- the most constrainted value set.
    root = mostConstraintedKey originalConstraints

    -- Recursive Definition
    selectCyclicArrangement' :: RandomGen gen => ConstraintMap -> Int -> Name -> gen -> Maybe CyclicArrangement
    selectCyclicArrangement' constraints depth key gen
      |  constraints == empty
      || not atTerminalDepth
      && null branches
      || atTerminalDepth
      && not validTerminalNode = Nothing
      |  atTerminalDepth
      &&     validTerminalNode = Just [key]
      | otherwise = prependKey
                  . coalesce
                  . map branchMay
                  $ zip branches' branchGens
      where
        atTerminalDepth         = depth == height
        validTerminalNode       = root `elem` originalRecipients
        originalRecipients      = originalConstraints ! key
        prependKey              = fmap (key:)
        branchMay               = uncurry $ selectCyclicArrangement' constraints' (depth + 1)
        branches                = constraints ! key
        branches'               = shuffle' branches branchCount shuffleGen
        branchCount             = length branches
        constraints'            = reduceReceipiants key constraints
        (branchGens,shuffleGen) = runState (replicateM branchCount (state split >> get)) gen

mostConstraintedKey :: ConstraintMap -> Name
mostConstraintedKey = fst
                    . minimumBy (comparing (length . snd))
                    . assocs

reduceReceipiants :: Name -> ConstraintMap -> ConstraintMap
reduceReceipiants key = fmap (delete key)

coalesce :: [Maybe a] -> Maybe a
coalesce = foldr (<|>) Nothing
