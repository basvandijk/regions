{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Region.Concurrent
-- Copyright   :  (c) 2009-2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- Concurrently executing regions.
--
-- This module exports functions with equivalent names from @Control.Concurrent@
-- and @GHC.Conc@. May I suggest you import this module qualified as in:
--
-- @import qualified Control.Monad.Trans.Region.Concurrent as Region@
--
--------------------------------------------------------------------------------

module Control.Monad.Trans.Region.Concurrent
    ( forkIO
    , forkOS
#ifdef __GLASGOW_HASKELL__
    , forkOnIO
#endif
    ) where

import Control.Monad.Trans.Region.Internal ( forkIO, forkOS, forkOnIO )
