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
-- This module should /only/ be used by library authors wishing to allow their
-- end-users to open their resources in a region.
--
-- The only thing to do, to create a module or library that allows your users to
-- open your resources in a region, is to define an instance for 'Resource' for
-- your type of resource.
--
-- Make sure not to re-export anything from this module. Either re-export things
-- from @Control.Monad.Trans.Region@ or tell your users to import that module
-- directly.
--
--------------------------------------------------------------------------------

module Control.Monad.Trans.Region.Unsafe
    ( -- * Scarce resources
      Resource
    , Handle
    , openResource
    , closeResource

      -- * Accessing the internal handle of a resource.
    , internalHandle

      -- * Duplication
    , Dup
    , dup

      -- * Parent/child relationship between regions.
    , ParentOf
    ) where

import Control.Monad.Trans.Region.Internal
