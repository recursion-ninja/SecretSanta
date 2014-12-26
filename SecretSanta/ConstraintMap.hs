module SecretSanta.ConstraintMap
  (constraintMap) where

import           Control.Arrow            ((&&&))
import           Data.Map          hiding (delete,filter,null)
import           Prelude           hiding (lookup)
import           SecretSanta.Types


-- | Takes a list of participants,
-- |       a list of arrangements,
-- |   and a list of validation functions 
-- | to produce a constraint mapping
constraintMap :: [Participant] -> [Arrangement] -> ConstraintMap
constraintMap xs ys = fromList constraints'
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


{-- Should start using constraint functions like this
notGrandparentOf :: [Arrangement] -> Participant -> Participant -> Bool
notGrandparentOf _ x y = (generation x /= Grandparent)
                      || (generation y /= Grandchild) 
--}
