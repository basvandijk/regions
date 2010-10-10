{-# LANGUAGE UnicodeSyntax #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Region.Unsafe
-- Copyright   :  (c) 2009-2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module is not intended for end-users. It should only be used by library
-- authors wishing to extend this @regions@ library.
--
--------------------------------------------------------------------------------

module Control.Monad.Trans.Region.OnExit ( Finalizer
                                         , FinalizerHandle
                                         , onExit
                                         ) where

import Control.Monad.Trans.Region.Internal ( Finalizer
                                           , FinalizerHandle
                                           , onExit
                                           )
