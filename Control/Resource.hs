{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, TypeFamilies #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Resource
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- Unsafely opening and closing of scarce resources.
--
--------------------------------------------------------------------------------

module Control.Resource
    ( Resource
    , Handle
    , openResource
    , closeResource
    ) where

import System.IO ( IO )

{-| Class of /scarce/ resources. A scarce resource is a resource that only one
user can use at a time. (like a file, memory pointer or USB device for
example).

Because of the scarcity, these resources need to be /opened/ to grant temporary
sole access to the resource. When the resource is no longer needed it should be
/closed/ a.s.a.p to grant others access to the resource.
-}
class Resource resource where
    data Handle resource ∷ *

    -- | Yield an @IO@ computation that opens the given resource and returns a
    -- @Handle@ on it.
    --
    -- Think of a handle as your private session with the resource. As long as
    -- you have it, other users can't use the resource. So don't forget to close
    -- this session with the resource as soon as you're done with it.
    openResource  ∷ resource → IO (Handle resource)

    -- | Yield an @IO@ computation that closes the resource identified by the
    -- given @Handle@.
    --
    -- /Warning/: Using the @Handle@ after the resource has been closed will
    -- usually throw an exception or result in a program crash!
    closeResource ∷ Handle resource → IO ()
