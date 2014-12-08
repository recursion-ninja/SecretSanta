module SecretSanta.ConstraintMap
  (constraintMap) where

import           Control.Arrow            ((&&&))
import           Data.Map          hiding (delete,filter,null)
import           Prelude           hiding (lookup)
import           SecretSanta.Types

constraintMap :: [Participant] -> [Arrangement] -> Maybe ConstraintMap
constraintMap xs ys
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
