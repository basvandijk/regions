{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Region.Concurrent
-- Copyright   :  (c) 2009-2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- Forking /top-level/ regions
--
--------------------------------------------------------------------------------

module Control.Monad.Trans.Region.Concurrent
    ( forkIOTopRegion
    , forkOSTopRegion
#ifdef __GLASGOW_HASKELL__
    , forkOnIOTopRegion
#endif
    ) where

import Control.Monad.Trans.Region.Internal
