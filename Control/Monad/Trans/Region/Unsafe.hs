{-# LANGUAGE UnicodeSyntax #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Region.Unsafe
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- /WARNING:/ This module should /not/ be used by end-users directly because it
-- allows access to the 'internalHandle' of a resource which enables them to
-- close the resource manually, which will defeat the safety-guarantees that
-- this package provides!
--
--------------------------------------------------------------------------------

module Control.Monad.Trans.Region.Unsafe
    ( internalHandle
    , mapInternalHandle
    ) where

import Control.Monad.Trans.Region.Internal ( RegionalHandle, internalHandle )
import Control.Resource ( Handle )

-- | Modify the internal handle from the given regional handle.
mapInternalHandle ∷ (Handle resource1 → Handle resource2)
                  → (RegionalHandle resource1 r → RegionalHandle resource2 r)
mapInternalHandle f rh = rh { internalHandle = f $ internalHandle rh }
