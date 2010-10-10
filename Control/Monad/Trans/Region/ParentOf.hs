{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , KindSignatures
           , MultiParamTypeClasses
           , UndecidableInstances
           , FlexibleInstances
           , OverlappingInstances
           , TypeFamilies
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Region.ParentOf
-- Copyright   :  (c) 2009-2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
--------------------------------------------------------------------------------

module Control.Monad.Trans.Region.ParentOf ( ParentOf ) where

-- from regions:
import Control.Monad.Trans.Region.Internal ( RegionT )

{-| The @ParentOf@ class defines the parent/child relationship between regions.
The constraint

@
    pr \`ParentOf\` cr
@

is satisfied if and only if @cr@ is a sequence of zero or more @'RegionT' s@
(with varying @s@) applied to @pr@, in other words, if @cr@ is an (improper)
nested subregion of @pr@.

The only purpose of the non-exported class 'Private' is to
make it impossible to add new instances of 'ParentOf', effectively turning it
into a /closed class/.
-}

-- Implementation note:
-- Since ghc-6.10.x we have type equality constraints, so we no longer need
-- Oleg's TypeCast tricks.
-- The implementation uses type-level recursion, so it is no surprise we need
-- UndecidableInstances.

class (Private pr, Private cr) ⇒ ParentOf (pr ∷ * → *) (cr ∷ * → *)

instance (Private pr) ⇒ ParentOf pr pr
instance (cr ~ RegionT s pcr, ParentOf pr pcr) ⇒ ParentOf pr cr

class Private r

instance Private (RegionT s pr)
