{-# LANGUAGE TypeSynonymInstances #-}
module SecretSanta.CyclicArangement () where

import Control.Applicative ((<$>),(<*>))
import Control.Arrow (second)
import Data.List     (delete,find,minimumBy)
import Data.Map      (Map,assocs)
import Data.Foldable (foldl')
import Data.Maybe    (fromJust,isJust,mapMaybe)
import Data.Ord      (comparing)
import Data.Tree
import SecretSanata.Types
import SecretSanata.ConstraintMap

instance Constrainable CyclicArrangement where
  feasibleArrangements = feasibleHamiltonianCycles

mapToTree :: Eq a => Map a [a] -> Tree a
mapToTree = constructNode
        <$> assocs
        <*> (minimumBy (comparing (length .snd)) . assocs)

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

cycles :: Int -> Tree a -> [[a]]
cycles 1 tree = [[rootLabel tree]]
cycles n tree
  | (null . subForest) tree = []
  | otherwise = concatMap
              ( fmap ((rootLabel tree) :)
              . filter (not.null)
              . cycles2 (n-1)
              ) $ subForest tree

feasibleCyclicArrangements :: Eq a => ConstraintMap -> [CyclicArranagement]
feasibleCyclicArrangements xs = cycles len $ mapToTree xs
  where len = foldl' (\x y -> x+1) 0 xs
