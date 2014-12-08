{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SecretSanta.Arrangement
  ( Arrangement
  , feasibleArrangements
  , selectArrangement
  ) where


import           SecretSanta.Types
--import           SecretSanta.ConstraintMap
--import           Control.Arrow           ((&&&),second)
--import qualified Control.Monad.Random    as R
--import           Data.Ord                (comparing)
--import           Data.List               (delete,minimumBy)
import           Data.Map hiding         (delete,filter,null)
--import           Data.Maybe              (isNothing)
import           Data.Traversable        (sequenceA)
import           Prelude hiding          (lookup)

instance Arrangementia Arrangement where
  feasibleArrangements = feasibleNormalArrangements

feasibleNormalArrangements :: ConstraintMap -> [Arrangement]
feasibleNormalArrangements = filter validArrangement . sequenceA
  where
    validArrangement = nodupes . elems
    nodupes []     = True
    nodupes (y:ys) = notElem y ys
                  && nodupes ys




{-
selectArrangement :: [Participant] -> [Arrangement] -> IO (Maybe Arrangement)
selectArrangement xs
  = sequenceA
  . fmap fromListR
  . feasibleArrangements xs
  where
    fromListR :: [a] -> IO a
    fromListR = R.fromList . flip zip (repeat 1)
-}

{-
feasibleArrangements :: [Participant] -> [Arrangement] -> Maybe [Arrangement]
feasibleArrangements xs ys
  = relax (arrangements xs) ys
  where
    relax :: ([a] -> Maybe b) -> [a] -> Maybe b
    relax f zs
      | null xs     = e
      | isNothing e = relax f $ tail zs
      | otherwise   = e
      where e = f zs
-}

{--
arrangements :: [Participant] -> [Arrangement] -> Maybe [Arrangement]
arrangements xs
  = nonEmpty
  . fmap (filter validArrangement . sequenceA)
  . constraintMap xs
  where
    nonEmpty  Nothing  = Nothing
    nonEmpty (Just []) = Nothing
    nonEmpty  x        = x
    validArrangement = nodupes . elems
    nodupes []     = True
    nodupes (y:ys) = notElem y ys
                  && nodupes ys
--}

{-
arrangementConstraints :: [Participant] -> [Arrangement] -> Maybe (Map Name [Name])
arrangementConstraints xs ys
  | any (null . snd) constraints' = Nothing
  | otherwise = Just $ fromList constraints'
  where
    constraints' = fmap (name &&& fmap name . possibleRecipients) xs
    possibleRecipients z = filter (satisfiesConstraints z) xs 
    satisfiesConstraints z
      = and . (\./) fmap
      [ not . grandparentOf     z
      , not . sameFamily        z
      , not . previousRecipient z
      ]
    grandparentOf z a
      = generation z == Grandparent
     && generation a == Grandchild
    sameFamily z a
      = family z == family a
    previousRecipient z a
      = any (isInMap z a) ys
    isInMap z a
      = (== Just True)
      . fmap (== name a)
      . lookup  (name z)

(\./) :: (((a -> c1) -> c1) -> b -> c) -> b -> a -> c
(\./) = flip . (. flip id) -- swing combinator

arrangementConstraints2 :: [(Name,[Name])] -> [(Name,Name)]
arrangementConstraints2 [] = [] 
arrangementConstraints2 xs = do 
  y <- ys
  (p,y) : (arrangementConstraints2 . fmap (second (delete y)) $ delete (p,ys) xs)
  where
    (p,ys) = minimumBy (comparing (length . snd)) xs
-}
