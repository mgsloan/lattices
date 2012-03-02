module Algebra.Lattice.Dropped (
    Dropped(..)
  ) where

import Algebra.Lattice

--
-- Dropped
--

-- | Graft a distinct top onto an unbounded lattice.
-- As a bonus, the top will be an absorbing element for the join.

data Dropped a = Top
               | Drop a
  deriving (Eq, Show, Read)

instance JoinSemiLattice a => JoinSemiLattice (Dropped a) where
    Top    `join` _      = Top
    _      `join` Top    = Top
    Drop x `join` Drop y = Drop (x `join` y)

instance MeetSemiLattice a => MeetSemiLattice (Dropped a) where
    Top    `meet` drop_y = drop_y
    drop_x `meet` Top    = drop_x
    Drop x `meet` Drop y = Drop (x `meet` y)

instance Lattice a => Lattice (Dropped a) where

instance BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Dropped a) where
    bottom = Drop bottom

instance MeetSemiLattice a => BoundedMeetSemiLattice (Dropped a) where
    top = Top

{- TODO
instance (MeetSemiLattice a, BoundedJoinSemiLattice a) => BoundedLattice (Dropped a) where

-- TODO: This is masking the normal top, which is 
instance (BooleanLattice a, Eq a) => BooleanLattice a where
    complement (Drop x)
      | x == bottom = Top
      | x           = x
    complement Top  = Drop bottom
    complement x    = complement x

instance (JoinSemiLattice a, Monoid a) => ResiduatedLattice a where
    residueL = 
    residueR = 

class BoundedLattice a => DiffLattice a where
    diff Top x = 
    diff x Top = 
    diff x y = 
-}