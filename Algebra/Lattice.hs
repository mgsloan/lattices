{-# LANGUAGE FlexibleInstances #-}
module Algebra.Lattice (
    -- * Unbounded lattices
    JoinSemiLattice(..), MeetSemiLattice(..), Lattice,
    joinLeq, joins1, meetLeq, meets1,
    
    -- * Bounded lattices
    BoundedJoinSemiLattice(..), BoundedMeetSemiLattice(..), BoundedLattice,
    joins, meets,

    -- * Boolean lattices
    BooleanLattice(..),

    -- * Lattices supporting difference
    ResiduatedLattice(..), DiffLattice(..),
    
    -- * Fixed points of chains in lattices
    lfp, lfpFrom, unsafeLfp,
    gfp, gfpFrom, unsafeGfp,
  ) where

import Algebra.Enumerable
import qualified Algebra.PartialOrd as PO

import Data.Monoid (Monoid)
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.IntMap as IM

-- | A algebraic structure with element joins: <http://en.wikipedia.org/wiki/Semilattice>
--
-- Associativity: x `join` (y `join` z) == (x `join` y) `join` z
-- Commutativity: x `join` y == y `join` x
-- Idempotency:   x `join` x == x
--
-- Partial-Order: x `leq` y == (x `join` y == y)
class JoinSemiLattice a where
    join :: a -> a -> a

-- | The partial ordering induced by the join-semilattice structure
joinLeq :: (Eq a, JoinSemiLattice a) => a -> a -> Bool
joinLeq x y = x `join` y == y

-- | The join of at a list of join-semilattice elements (of length at least one)
joins1 :: JoinSemiLattice a => [a] -> a
joins1 = foldr1 join

-- | A algebraic structure with element meets: <http://en.wikipedia.org/wiki/Semilattice>
--
-- Associativity: x `meet` (y `meet` z) == (x `meet` y) `meet` z
-- Commutativity: x `meet` y == y `meet` x
-- Idempotency:   x `meet` x == x
--
-- Partial-Order: x `leq` y == (x `meet` y == x)
class MeetSemiLattice a where
    meet :: a -> a -> a

-- | The partial ordering induced by the meet-semilattice structure
meetLeq :: (Eq a, MeetSemiLattice a) => a -> a -> Bool
meetLeq x y = x `meet` y == x

-- | The meet of at a list of meet-semilattice elements (of length at least one)
meets1 :: MeetSemiLattice a => [a] -> a
meets1 = foldr1 meet

-- | The combination of two semi lattices makes a lattice if the absorption law holds:
-- see <http://en.wikipedia.org/wiki/Absorption_law> and <http://en.wikipedia.org/wiki/Lattice_(order)>
--
-- Absorption: a `join` (a `meet` b) == a `meet` (a `join` b) == a
class (JoinSemiLattice a, MeetSemiLattice a) => Lattice a where

-- | A join-semilattice with some element |bottom| that `join` approaches.
--
-- Identity: x `join` bottom == x
class JoinSemiLattice a => BoundedJoinSemiLattice a where
    bottom :: a

-- | The join of a list of join-semilattice elements
joins :: BoundedJoinSemiLattice a => [a] -> a
joins = foldr join bottom

-- | A meet-semilattice with some element |top| that `meet` approaches.
--
-- Identity: x `meet` top == x
class MeetSemiLattice a => BoundedMeetSemiLattice a where
    top :: a

-- | The meet of a list of meet-semilattice elements
meets :: BoundedMeetSemiLattice a => [a] -> a
meets = foldr meet top

-- | Lattices with both bounds
--
-- Partial-Order: bottom `leq` top
class (Lattice a, BoundedJoinSemiLattice a, BoundedMeetSemiLattice a) => BoundedLattice a where

-- Constraint Kinds:  type BoundedLattice a = (Lattice a, BoundedJoinSemiLattice a, BoundedMeetSemiLattice a)


-- | Boolean lattices have a complement and are distributive over join and meet:
--   <http://en.wikipedia.org/wiki/Complemented_lattice>
--
-- Complement Join: complement x `join` x == top
-- Complement Meet: complement x `meet` x == bottom
-- Involution: complement (complement x) == x
-- Order-Reversing: complement x `leq` complement y == y `leq` x
class BoundedLattice a => BooleanLattice a where
    complement :: a -> a

-- | Lattices with residuals for the Monoid: <http://en.wikipedia.org/wiki/Residuated_lattice>
--
-- TODO: MeetSemiLattice variant?
--
-- (y `leq` residueR x z) === (mappend x y `leq` z) === (x `leq` residueL z y)
class (JoinSemiLattice a, Monoid a) => ResiduatedLattice a where
    residualL, residualR :: a -> a -> a

-- | Lattices with implication - Heyting Algebras: <http://en.wikipedia.org/wiki/Heyting_algebra>
--
-- When the Monoid operations are the same as the BoundedJoinSemiLattice,
-- residueL === residueR === diff
--
-- Partial-Order: a `leq` b == (a `diff` b == top)
-- Distributive: a `diff` (b `join` c) == (a `diff` b) `join` (a `diff` c)
-- a `join` (a `diff` b) == a `join` b
-- b `join` (a `diff` b) == b
class BoundedLattice a => DiffLattice a where
    diff :: a -> a -> a


--
-- Sets
--

instance Ord a => JoinSemiLattice (S.Set a) where
    join = S.union

instance Ord a => MeetSemiLattice (S.Set a) where
    meet = S.intersection

instance Ord a => Lattice (S.Set a) where

instance Ord a => BoundedJoinSemiLattice (S.Set a) where
    bottom = S.empty

instance (Ord a, Enumerable a) => BoundedMeetSemiLattice (S.Set a) where
    top = S.fromList universe

instance (Ord a, Enumerable a) => BoundedLattice (S.Set a) where

instance Ord a => ResiduatedLattice (S.Set a) where
    residualL = S.difference
    residualR = S.difference

instance (Ord a, Enumerable a) => DiffLattice (S.Set a) where
    diff = S.difference

--
-- IntSets
--

instance JoinSemiLattice IS.IntSet where
    join = IS.union

instance BoundedJoinSemiLattice IS.IntSet where
    bottom = IS.empty

instance ResiduatedLattice IS.IntSet where
    residualL = IS.difference
    residualR = IS.difference

--
-- Maps
--

instance (Ord k, JoinSemiLattice v) => JoinSemiLattice (M.Map k v) where
    join = M.unionWith join

instance (Ord k, MeetSemiLattice v) => MeetSemiLattice (M.Map k v) where
    meet = M.intersectionWith meet

instance (Ord k, Lattice v) => Lattice (M.Map k v) where

instance (Ord k, JoinSemiLattice v) => BoundedJoinSemiLattice (M.Map k v) where
    bottom = M.empty

instance (Ord k, Enumerable k, BoundedMeetSemiLattice v) => BoundedMeetSemiLattice (M.Map k v) where
    top = M.fromList (universe `zip` repeat top)

instance (Ord k, Enumerable k, BoundedLattice v) => BoundedLattice (M.Map k v) where

instance (Ord k, Enumerable k, BooleanLattice v) => BooleanLattice (M.Map k v) where
    complement = M.unionWith (const id) top . M.map complement

instance (Ord k, ResiduatedLattice v) => ResiduatedLattice (M.Map k v) where
    residualL = M.differenceWith (\x y -> Just $ residualL x y)
    residualR = M.differenceWith (\x y -> Just $ residualR x y)

instance (Ord k, Enumerable k, DiffLattice v) => DiffLattice (M.Map k v) where
    diff      = M.differenceWith (\x y -> Just $ diff x y)

--
-- IntMaps
--

instance JoinSemiLattice v => JoinSemiLattice (IM.IntMap v) where
    join = IM.unionWith join

instance MeetSemiLattice v => MeetSemiLattice (IM.IntMap v) where
    meet = IM.intersectionWith meet

instance Lattice v => Lattice (IM.IntMap v) where

instance JoinSemiLattice v => BoundedJoinSemiLattice (IM.IntMap v) where
    bottom = IM.empty

instance ResiduatedLattice v => ResiduatedLattice (IM.IntMap v) where
    residualL = IM.differenceWith (\x y -> Just $ residualL x y)
    residualR = IM.differenceWith (\x y -> Just $ residualR x y)

--
-- Functions
--

instance JoinSemiLattice v => JoinSemiLattice (k -> v) where
    f `join` g = \x -> f x `join` g x

instance MeetSemiLattice v => MeetSemiLattice (k -> v) where
    f `meet` g = \x -> f x `meet` g x

instance Lattice v => Lattice (k -> v) where

instance BoundedJoinSemiLattice v => BoundedJoinSemiLattice (k -> v) where
    bottom = const bottom

instance BoundedMeetSemiLattice v => BoundedMeetSemiLattice (k -> v) where
    top = const top

instance BoundedLattice v => BoundedLattice (k -> v) where

instance BooleanLattice v => BooleanLattice (k -> v) where
    complement = (complement .)

instance ResiduatedLattice v => ResiduatedLattice (k -> v) where
    residualL f g = \x -> residualL (f x) (g x)
    residualR f g = \x -> residualR (f x) (g x)

instance DiffLattice v => DiffLattice (k -> v) where
    diff      f g = \x -> diff (f x) (g x) 

--
-- Tuples
--

instance (JoinSemiLattice a, JoinSemiLattice b) => JoinSemiLattice (a, b) where
    (x1, y1) `join` (x2, y2) = (x1 `join` x2, y1 `join` y2)

instance (MeetSemiLattice a, MeetSemiLattice b) => MeetSemiLattice (a, b) where
    (x1, y1) `meet` (x2, y2) = (x1 `meet` x2, y1 `meet` y2)

instance (Lattice a, Lattice b) => Lattice (a, b) where

instance (BoundedJoinSemiLattice a, BoundedJoinSemiLattice b) => BoundedJoinSemiLattice (a, b) where
    bottom = (bottom, bottom)

instance (BoundedMeetSemiLattice a, BoundedMeetSemiLattice b) => BoundedMeetSemiLattice (a, b) where
    top = (top, top)

instance (BoundedLattice a, BoundedLattice b) => BoundedLattice (a, b) where

instance (BooleanLattice a, BooleanLattice b) => BooleanLattice (a, b) where
    complement (x, y) = (complement x, complement y)

instance (ResiduatedLattice a, ResiduatedLattice b) => ResiduatedLattice (a, b) where
    residualL (x1, y1) (x2, y2) = (residualL x1 x2, residualL y1 y2)
    residualR (x1, y1) (x2, y2) = (residualR x1 x2, residualR y1 y2)

instance (DiffLattice a, DiffLattice b) => DiffLattice (a, b) where
    diff (x1, y1) (x2, y2) = (diff x1 x2, diff y1 y2)


--
-- Bools
--

instance JoinSemiLattice Bool where
    join = (||)

instance MeetSemiLattice Bool where
    meet = (&&)

instance Lattice Bool where

instance BoundedJoinSemiLattice Bool where
    bottom = False

instance BoundedMeetSemiLattice Bool where
    top = True

instance BoundedLattice Bool where

instance BooleanLattice Bool where
    complement = not


--
-- Maybe
--

instance JoinSemiLattice (Maybe a) where


-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Assumes that the function is monotone and does not check if that is correct.
{-# INLINE unsafeLfp #-}
unsafeLfp :: (Eq a, BoundedJoinSemiLattice a) => (a -> a) -> a
unsafeLfp = PO.unsafeLfpFrom bottom

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Forces the function to be monotone.
{-# INLINE lfp #-}
lfp :: (Eq a, BoundedJoinSemiLattice a) => (a -> a) -> a
lfp = lfpFrom bottom

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Forces the function to be monotone.
{-# INLINE lfpFrom #-}
lfpFrom :: (Eq a, BoundedJoinSemiLattice a) => a -> (a -> a) -> a
lfpFrom init_x f = PO.unsafeLfpFrom init_x (\x -> f x `join` x)


-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Assumes that the function is antinone and does not check if that is correct.
{-# INLINE unsafeGfp #-}
unsafeGfp :: (Eq a, BoundedMeetSemiLattice a) => (a -> a) -> a
unsafeGfp = PO.unsafeGfpFrom top

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Forces the function to be antinone.
{-# INLINE gfp #-}
gfp :: (Eq a, BoundedMeetSemiLattice a) => (a -> a) -> a
gfp = gfpFrom top

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Forces the function to be antinone.
{-# INLINE gfpFrom #-}
gfpFrom :: (Eq a, BoundedMeetSemiLattice a) => a -> (a -> a) -> a
gfpFrom init_x f = PO.unsafeGfpFrom init_x (\x -> f x `meet` x)
