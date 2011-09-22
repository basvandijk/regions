-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Region.Unsafe
-- Copyright   :  (c) 2009-2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
--
--------------------------------------------------------------------------------

module Control.Monad.Trans.Region.Unsafe
    ( -- * Local regions
      unsafeStripLocal

      -- * MonadTransControl & MonadControlIO
    , unsafeLiftControl
    , unsafeLiftControlIO
    , unsafeControlIO
    , unsafeLiftIOOp
    , unsafeLiftIOOp_
    ) where

import Control.Monad.Trans.Region.Internal
