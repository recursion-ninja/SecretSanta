{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SecretSanta.CyclicArrangement
  ( CyclicArrangement
  , f 
  ) where

import Debug.Trace (trace)
import System.IO.Unsafe

import Control.Applicative ((<$>),(<*>),(<|>))
import Control.Arrow (second)
--import Control.Monad.Random
import Control.Monad (liftM)
import Data.List     (delete,find,minimumBy)
import Data.Map      ((!),assocs,fromList)
import Data.Foldable (foldl')
import Data.Maybe    (mapMaybe)
import Data.Ord      (comparing)
import Data.Tree
import SecretSanta.Types
import System.Random.Shuffle


-- | Represents a valid secret santa arrangement
-- | which is also a Hamiltonian Cycle
instance Constrainable CyclicArrangement where
  feasibleArrangements = feasibleCyclicArrangements
  toArrangement        = cycleToMap


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

{--}
f :: ConstraintMap -> IO (Maybe CyclicArrangement)
f xs = shuffleM xs'
   >>= fmap coalesce
     . (\x -> trace (show $ unsafePerformIO x) x)
     . mapM ( liftM . liftM . (:)
          <$> fst
          <*> (top <$> fst
                   <*> const depth
                   <*> flip  strip xs' . fst))

  where
    xs'   = assocs xs
    depth = length xs' - 1
    top :: Name -> Int -> [(Name,[Name])] -> IO (Maybe CyclicArrangement)
    top _ _ [] = return Nothing
    top e 1 m
      | e `elem` options = return $ Just [lastPerson]
      | otherwise        = return Nothing
      where
        options    = xs ! lastPerson
        lastPerson = fst $ head m
    top e n m
        = shuffleM m
      >>= fmap coalesce
        . mapM (liftM . liftM . (:)
            <$> fst
            <*>
                    (top <$> const e
                         <*> const (n-1)
                         <*> flip strip m . fst)
               )

strip :: Name -> [(Name,[Name])] -> [(Name,[Name])]
strip x = fmap (second (delete x)) . filter ((/=x).fst)

coalesce = foldr (<|>) Nothing

{--}
