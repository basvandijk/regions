-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Region
-- Copyright   :  (c) 2009-2011 Bas van Dijk
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

    , RegionBaseControl

      -- * Duplication
    , Dup(dup)

      -- * Ancestor relation between regions
    , AncestorRegion

      -- * Special regions
      -- ** The root region
    , RootRegion

      -- ** Local regions
    , LocalRegion, Local

      -- * Utilities for writing monadic instances
    , liftCallCC
    , mapRegionT
    , liftCatch
    ) where

import Control.Monad.Trans.Region.Internal

