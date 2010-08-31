{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Region
-- Copyright   :  (c) 2009-2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This modules implements a technique called /"Lightweight monadic regions"/
-- invented by Oleg Kiselyov and Chung-chieh Shan
--
-- See: <http://okmij.org/ftp/Haskell/regions.html#light-weight>
--
--------------------------------------------------------------------------------

module Control.Monad.Trans.Region
    ( -- * Regions
      RegionT

      -- * Running regions
    , runRegionT

    , TopRegion
    , runTopRegion

      -- ** Forking /top-level/ regions
    , forkIOTopRegion
    , forkOSTopRegion
#ifdef __GLASGOW_HASKELL__
    , forkOnIOTopRegion
#endif
      -- * Duplication
    , Dup(dup)

    , module Control.Monad.Trans.Region.ParentOf

      -- * Handy functions for writing monadic instances
    , liftCallCC
    , mapRegionT
    , liftCatch
    ) where

import Control.Monad.Trans.Region.Internal
import Control.Monad.Trans.Region.ParentOf
