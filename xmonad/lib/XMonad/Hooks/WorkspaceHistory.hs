-- The following code is taken from the darcs version of WorkspaceHistory,
-- and made available here under this licence:

-- Copyright (c) The Xmonad Community
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.


{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.WorkspaceHistory
-- Copyright   :  (c) 2013 Dmitri Iouchtchenko
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Dmitri Iouchtchenko <johnnyspoon@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Keeps track of workspace viewing order.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.WorkspaceHistory
    ( -- * Usage
      -- $usage

      -- * Hooking
      workspaceHistoryHook

      -- * Querying
    , workspaceHistory

    ) where

import XMonad
import XMonad.StackSet (currentTag)
import qualified XMonad.Util.ExtensibleState as XS

-- $usage
-- To record the order in which you view workspaces, you can use this
-- module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
--
-- Then add the hook to your 'logHook':
--
-- >  main = xmonad $ defaultConfig
-- >      { ...
-- >      , logHook = ... >> workspaceHistoryHook >> ...
-- >      , ...
-- >      }
--
-- To make use of the collected data, a query function is provided.

data WorkspaceHistory =
    WorkspaceHistory { history :: [WorkspaceId] -- ^ Workspaces in reverse-chronological order.
                     }
    deriving (Typeable, Read, Show)

instance ExtensionClass WorkspaceHistory where
    initialValue = WorkspaceHistory []
    extensionType = PersistentExtension

-- | A 'logHook' that keeps track of the order in which workspaces have
-- been viewed.
workspaceHistoryHook :: X ()
workspaceHistoryHook = gets (currentTag . windowset) >>= (XS.modify . makeFirst)

-- | A list of workspace tags in the order they have been viewed, with the
-- most recent first. No duplicates are present, but not all workspaces are
-- guaranteed to appear, and there may be workspaces that no longer exist.
workspaceHistory :: X [WorkspaceId]
workspaceHistory = XS.gets history


-- | Cons the 'WorkspaceId' onto the 'WorkspaceHistory' if it is not
-- already there, or move it to the front if it is.
makeFirst :: WorkspaceId -> WorkspaceHistory -> WorkspaceHistory
makeFirst w v = let (xs, ys) = break (w ==) $ history v
                in v { history = w : (xs ++ drop 1 ys) }
