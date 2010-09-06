{-# LANGUAGE CPP
           , UnicodeSyntax
           , NoImplicitPrelude
           , GeneralizedNewtypeDeriving
           , RankNTypes
           , KindSignatures
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Region.Internal
-- Copyright   :  (c) 2009-2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This modules implements a technique called /"Lightweight monadic regions"/
-- invented by Oleg Kiselyov and Chung-chieh Shan
--
-- See: <http://okmij.org/ftp/Haskell/regions.html#light-weight>
--
-- This is an internal module not intended to be exported.
-- Use @Control.Monad.Trans.Region@  or @Control.Monad.Trans.Region.Unsafe@.
--
--------------------------------------------------------------------------------

module Control.Monad.Trans.Region.Internal
    ( -- * Regions
      RegionT

      -- * Close handles
    , CloseAction
    , CloseHandle
    , onExit

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

      -- * Handy functions for writing monadic instances
    , liftCallCC
    , mapRegionT
    , liftCatch
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude             ( (+), (-), seq, fromInteger )
import Control.Applicative ( Applicative, Alternative )
import Control.Monad       ( Monad, return, (>>=), fail
                           , (>>), when, forM_
                           , MonadPlus
                           )
import Control.Monad.Fix   ( MonadFix )
import System.IO           ( IO )
import Data.Function       ( ($) )
import Data.Functor        ( Functor )
import Data.Int            ( Int )
import Data.IORef          ( IORef, newIORef
                           , readIORef, modifyIORef, atomicModifyIORef
                           )
import Control.Concurrent  ( forkIO, forkOS, ThreadId )
#ifdef __GLASGOW_HASKELL__
import GHC.Conc            ( forkOnIO )
#endif

-- from MonadCatchIO-transformers:
import Control.Monad.CatchIO ( MonadCatchIO, block, bracket )

-- from transformers:
import Control.Monad.Trans.Class ( MonadTrans, lift )
import Control.Monad.IO.Class    ( MonadIO, liftIO )

import qualified Control.Monad.Trans.Reader as R ( liftCallCC, liftCatch )
import           Control.Monad.Trans.Reader      ( ReaderT(ReaderT)
                                                 , runReaderT, mapReaderT
                                                 )
-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )


--------------------------------------------------------------------------------
-- * Regions
--------------------------------------------------------------------------------

{-| A monad transformer in which scarce resources can be opened
which are automatically closed when the region terminates.

Note that regions can be nested. @pr@ (for parent region) is a monad which is
usually the region which is running this region. However when you are running a
'TopRegion' the parent region will be 'IO'.
-}
newtype RegionT s (pr ∷ * → *) α = RegionT
    { unRegionT ∷ ReaderT (IORef [Handle]) pr α }

    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadPlus
             , MonadFix
             , MonadTrans
             , MonadIO
             , MonadCatchIO
             )

data Handle = Handle !CloseAction !(IORef RefCnt)

-- | An 'IO' computation that closes or finalizes a resource. For example
-- @hClose@ or @free@.
type CloseAction = IO ()

type RefCnt = Int


--------------------------------------------------------------------------------
-- * Close handles
--------------------------------------------------------------------------------

-- | A handle to a 'CloseAction' that allows you to duplicate the action to a
-- parent region using 'dup'.
newtype CloseHandle (r ∷ * → *) = CloseHandle Handle

-- | Register the 'CloseAction' in the region. When the region terminates all
-- registered close actions will be perfomed if they're not duplicated to a
-- parent region.
onExit ∷ MonadIO pr ⇒ CloseAction → RegionT s pr (CloseHandle (RegionT s pr))
onExit closeAction = RegionT $ ReaderT $ \hsIORef → liftIO $ do
                       refCntIORef ← newIORef 1
                       let h = Handle closeAction refCntIORef
                       modifyIORef hsIORef (h:)
                       return $ CloseHandle h


--------------------------------------------------------------------------------
-- * Running regions
--------------------------------------------------------------------------------

{-| Execute a region inside its parent region @pr@.

All resources which have been opened in the given region and which haven't been
duplicated using 'dup', will be closed on exit from this function wether by
normal termination or by raising an exception.

Also all resources which have been duplicated to this region from a child region
are closed on exit if they haven't been duplicated themselves.

Note the type variable @s@ of the region wich is only quantified over the region
itself. This ensures that /all/ values, having a type containing @s@, can /not/
be returned from this function. (Note the similarity with the @ST@ monad.)

Note that it is possible to run a region inside another region.
-}
runRegionT ∷ MonadCatchIO pr ⇒ (∀ s. RegionT s pr α) → pr α
runRegionT m = runRegionWith [] m

{-| A region which has 'IO' as its parent region which enables it to be:

 * directly executed in 'IO' by 'runTopRegion',

 * concurrently executed in a new thread by 'forkIOTopRegion'.
-}
type TopRegion s = RegionT s IO

{-| Convenience funtion for running a /top-level/ region in 'IO'.

Note that: @runTopRegion = 'runRegionT'@
-}
runTopRegion ∷ (∀ s. TopRegion s α) → IO α
runTopRegion = runRegionT


--------------------------------------------------------------------------------
-- ** Forking /top-level/ regions
--------------------------------------------------------------------------------

{-| Return a region which executes the given /top-level/ region in a new thread.

Note that the forked region has the same type variable @s@ as the resulting
region. This means that all values which can be referenced in the resulting
region can also be referenced in the forked region.

For example the following is allowed:

@
'runRegionT' $ do
  regionalHndl <- open resource
  threadId <- 'forkIOTopRegion' $ doSomethingWith regionalHndl
  doSomethingElseWith regionalHndl
@

Note that the @regionalHndl@ and all other resources opened in the current
thread are closed only when the current thread or the forked thread terminates
whichever comes /last/.
-}
forkIOTopRegion ∷ MonadIO pr ⇒ TopRegion s () → RegionT s pr ThreadId
forkIOTopRegion = forkTopRegion forkIO

-- | Like 'forkIOTopRegion' but internally uses 'forkOS'.
forkOSTopRegion ∷ MonadIO pr ⇒ TopRegion s () → RegionT s pr ThreadId
forkOSTopRegion = forkTopRegion forkOS

#ifdef __GLASGOW_HASKELL__
-- | Like 'forkIOTopRegion' but internally uses 'forkOnIO'.
forkOnIOTopRegion ∷ MonadIO pr ⇒ Int → TopRegion s () → RegionT s pr ThreadId
forkOnIOTopRegion = forkTopRegion ∘ forkOnIO
#endif

forkTopRegion ∷ MonadIO pr
              ⇒ (IO () → IO ThreadId)
              → (TopRegion s () → RegionT s pr ThreadId)
forkTopRegion doFork = \m →
    RegionT $ ReaderT $ \hsIORef → liftIO $ do
      hs ← readIORef hsIORef
      block $ do
        forM_ hs $ \(Handle _ refCntIORef) → increment refCntIORef
        doFork $ runRegionWith hs m

--------------------------------------------------------------------------------

-- | Internally used function that actually runs the region on the given list of
-- opened resources.
runRegionWith ∷ ∀ s pr α. MonadCatchIO pr
              ⇒ [Handle] → RegionT s pr α → pr α
runRegionWith hs r = bracket (liftIO before)
                             (liftIO ∘ after)
                             (runReaderT $ unRegionT r)
    where
      before = newIORef hs
      after hsIORef = do
        hs' ← readIORef hsIORef
        forM_ hs' $ \(Handle closeAction refCntIORef) → do
          refCnt ← decrement refCntIORef
          when (refCnt ≡ 0) closeAction

-- | Internally used function that atomically decrements the reference count
-- that is stored in the given @IORef@. The function returns the decremented
-- reference count.
decrement ∷ IORef RefCnt → IO RefCnt
decrement ioRef = do atomicModifyIORef ioRef $ \refCnt →
                       let refCnt' = refCnt - 1
                       in (refCnt', refCnt')

-- | Internally used function that atomically increments the reference count that
-- is stored in the given @IORef@.
increment ∷ IORef RefCnt → IO ()
increment ioRef = do refCnt' ← atomicModifyIORef ioRef $ \refCnt →
                                 let refCnt' = refCnt + 1
                                 in (refCnt', refCnt')
                     refCnt' `seq` return ()


--------------------------------------------------------------------------------
-- * Duplication
--------------------------------------------------------------------------------

{-| Duplicate an @&#945;@ in the parent region. This @&#945;@ will usually be
some type of regional handle.

For example, suppose you run the following region:

@
runRegionT $ do
@

Inside this region you run a nested /child/ region like:

@
    r1hDup <- runRegionT $ do
@

Now in this child region you open the resource @r1@:

@
        r1h <- open r1
@

...yielding the regional handle @r1h@. Note that:

@r1h :: RegionalHandle (RegionT cs (RegionT ps ppr))@

where @cs@ is bound by the inner (child) @runRegionT@ and @ps@ is
bound by the outer (parent) @runRegionT@.

Suppose you want to use the resulting regional handle @r1h@ in the /parent/
region. You can't simply @return r1h@ because then the type variable @cs@,
escapes the inner region.

However, if you duplicate the regional handle you can safely return it.

@
        r1hDup <- dup r1h
        return r1hDup
@

Note that @r1hDup :: RegionalHandle (RegionT ps ppr)@

Back in the parent region you can safely operate on @r1hDup@.
-}
class Dup α where
    dup ∷ MonadCatchIO ppr
        ⇒ α (RegionT cs (RegionT ps ppr))
        → RegionT cs (RegionT ps ppr)
              (α (RegionT ps ppr))

instance Dup CloseHandle where
    dup (CloseHandle h@(Handle _ refCntIORef)) =
      lift $ RegionT $ ReaderT $ \hsIORef → liftIO $ block $ do
        increment refCntIORef
        modifyIORef hsIORef (h:)
        return $ CloseHandle h


--------------------------------------------------------------------------------
-- * Handy functions for writing monadic instances
--------------------------------------------------------------------------------

-- | Lift a @callCC@ operation to the new monad.
liftCallCC ∷ (((α → pr β) → pr α) → pr α)
           → (((α → RegionT s pr β) → RegionT s pr α) → RegionT s pr α)
liftCallCC callCC = \f → RegionT $ R.liftCallCC callCC $ unRegionT ∘ f ∘ (RegionT ∘)

-- | Transform the computation inside a region.
mapRegionT ∷ (m α → n β)
           → (RegionT s m α → RegionT s n β)
mapRegionT f = RegionT ∘ mapReaderT f ∘ unRegionT

-- | Lift a @catchError@ operation to the new monad.
liftCatch ∷ (pr α → (e → pr α) → pr α)
          → (RegionT s pr α → (e → RegionT s pr α) → RegionT s pr α)
liftCatch f = \m h → RegionT $ R.liftCatch f (unRegionT m) (unRegionT ∘ h)


-- The End ---------------------------------------------------------------------
