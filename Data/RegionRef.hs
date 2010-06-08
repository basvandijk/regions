{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, KindSignatures #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Data.RegionRef
-- Copyright   :  (c) 2009-2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- A mutable variable in the region monad that should provide a safer
-- replacement for an 'IORef' because you can't use it outside the region in
-- which it was created (unless you explicitly duplicate it).
--
--------------------------------------------------------------------------------

module Data.RegionRef
    ( RegionRef
    , pureDup
    , newRegionRef
    , readRegionRef
    , writeRegionRef
    , modifyRegionRef
    , atomicModifyRegionRef
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function ( ($) )
import Control.Monad ( liftM )
import Data.Eq       ( Eq )
import Data.IORef    ( IORef
                     , newIORef
                     , readIORef, writeIORef
                     , modifyIORef, atomicModifyIORef
                     )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers:
import Control.Monad.IO.Class ( MonadIO, liftIO )

-- from ourselves:
import Control.Monad.Trans.Region ( RegionT, ParentOf )


--------------------------------------------------------------------------------
-- Mutable references in the region monad
--------------------------------------------------------------------------------

-- | A mutable variable storing a value of type @&#945;@ which can only be used
-- in region @r@, @r@'s children or @r@'s parent when you duplicate it using
-- 'pureDup'.
newtype RegionRef (r ∷ * → *) α = RegionRef { unRegionRef ∷ IORef α }
    deriving ( Eq )

-- | Transform the region type @r@ of the regional reference to @r@'s parent so
-- that it can be used in that region.
pureDup ∷ RegionRef (RegionT cs (RegionT ps ppr)) α
        → RegionRef             (RegionT ps ppr)  α
pureDup = RegionRef ∘ unRegionRef

{-| Yield a regional computation that returns a new regional reference that
stores the given value.

Note that the reference is parameterized by the same region in which it was
created. This ensures you can never use this reference outside that region and
it allows you to use this reference in a child region of that region
-}
newRegionRef ∷ MonadIO pr
             ⇒ α → RegionT s pr (RegionRef (RegionT s pr) α)
newRegionRef = liftM RegionRef ∘ liftIO ∘ newIORef

-- | Read the value of the given regional reference.
readRegionRef ∷ (pr `ParentOf` cr, MonadIO cr)
              ⇒ RegionRef pr α → cr α
readRegionRef = liftIO ∘ readIORef ∘ unRegionRef

-- | Write a new value into the given regional reference.
writeRegionRef ∷ (pr `ParentOf` cr, MonadIO cr)
               ⇒ RegionRef pr α → α → cr ()
writeRegionRef ref x = liftIO $ writeIORef (unRegionRef ref) x

-- | Mutate the contents of the given regional reference using the given
-- function.
modifyRegionRef ∷ (pr `ParentOf` cr, MonadIO cr)
                ⇒ RegionRef pr α → (α → α) → cr ()
modifyRegionRef ref f = liftIO $ modifyIORef (unRegionRef ref) f

{-| Atomically modifies the contents of the given regional reference using the
given function.

This function is useful for using a regional reference in a safe way in a
multithreaded program. If you only have one regional reference, then using
@atomicModifyRegionRef@ to access and modify it will prevent race conditions.
-}
atomicModifyRegionRef ∷ (pr `ParentOf` cr, MonadIO cr)
                      ⇒ RegionRef pr α → (α → (α, β)) → cr β
atomicModifyRegionRef ref f = liftIO $ atomicModifyIORef (unRegionRef ref) f


-- The End ---------------------------------------------------------------------
