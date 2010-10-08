{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , KindSignatures
           , MultiParamTypeClasses
           , FunctionalDependencies
           , UndecidableInstances
           , FlexibleInstances
           , OverlappingInstances
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

-- from base:
import Control.Monad ( Monad )

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

The only purpose of the non-exported classes 'Private' and 'Private2' are to
make it impossible to add new instances of 'ParentOf', effectively turning it
into a /closed class/.
-}
class (Monad pr, Monad cr) ⇒ pr `ParentOf` cr

instance Monad r ⇒ ParentOf r r

instance ( Monad cr
         , cr `TypeCast2` RegionT s pcr
         , pr `ParentOf` pcr
         )
         ⇒ ParentOf pr cr


--------------------------------------------------------------------------------
-- Type casting
--------------------------------------------------------------------------------

class TypeCast2     (a ∷ * → *) (b ∷ * → *) |   a → b,   b → a
class TypeCast2'  t (a ∷ * → *) (b ∷ * → *) | t a → b, t b → a
class TypeCast2'' t (a ∷ * → *) (b ∷ * → *) | t a → b, t b → a

instance TypeCast2'  () a b ⇒ TypeCast2    a b
instance TypeCast2'' t  a b ⇒ TypeCast2' t a b
instance TypeCast2'' () a a


-- The End ---------------------------------------------------------------------
