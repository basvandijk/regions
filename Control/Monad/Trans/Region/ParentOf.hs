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


{-| The @ParentOf@ class declares the parent/child relationship between regions.

A region is the parent of another region if they're either equivalent like:

@
RegionT ps pr  \`ParentOf\`  RegionT ps pr
@

or if it is the parent of the parent of the child like:

@
RegionT ps ppr \`ParentOf\` RegionT cs
                              (RegionT pcs
                                (RegionT ppcs
                                  (RegionT ps ppr)))
@
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
