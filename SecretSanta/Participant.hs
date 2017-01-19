module SecretSanta.Participant where

type Name = String

newtype Family = Family String
  deriving (Eq,Ord)
{-
data Generation
   = Grandparent
   | Child
   | Grandchild
   deriving (Eq,Ord,Read,Show)
-}
data Participant
   = Participant
   { name  :: !Name
--   , family     :: !Family
--   , generation :: !Generation
   , email :: !String
   , tags  :: ![String]
   } deriving (Eq,Ord,Show)
{-
instance Show Participant where
  show = name
-}

{-
instance Read Participant where
  readsPrec _ str
    = [ ( Participant w x y z, t'''') 
      | ("Participant", t) <- lex str
      , (w, t')            <- reads t
      , (x, t'')           <- reads t'
      , (y, t''')          <- reads t''
      , (z, t'''')         <- reads t'''
      ]

instance Read Family where
  readsPrec _ str
    = [ (Family x, t') 
      | ("Family", t ) <- lex str
      , (x,t')         <- reads t
      ]

instance Show Family where
  show (Family f) = f
-}
