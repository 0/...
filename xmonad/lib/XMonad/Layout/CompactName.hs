{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | Compactor for layout names.
module XMonad.Layout.CompactName
    ( -- * Usage
      -- $usage
      compactName
    , CompactName
    ) where

import qualified XMonad as X

import qualified XMonad.Layout.LayoutModifier as LMod


-- $usage
--
-- Add
--
-- > import XMonad.Layout.CompactName
--
-- to your @~\/.xmonad\/xmonad.hs@. You can then use 'compactName' to compress the
-- description of your layouts.
--
-- For example,
--
-- > myLayout = compactName $
-- >            tiled ||| Mirror tiled ||| Full
-- >              where
-- >                tiled = Tall 1 0.03 0.5
--
-- would result in layouts with the names \"T\", \"MT\", and \"F\".


-- | Applies the 'CompactName' layout modifier to a layout, compressing its
-- name.
compactName = LMod.ModifiedLayout CompactName

data CompactName a = CompactName
    deriving (Show, Read, Eq)

instance LMod.LayoutModifier CompactName X.Window where
    modifyDescription _ = compress . words . X.description
      where compress :: [String] -> String
            compress ("Full":xs)              = 'F' : compress xs
            compress ("Magnifier":"(off)":xs) = compress xs
            compress ("Magnifier":xs)         = 'Z' : compress xs
            compress ("Mirror":xs)            = 'M' : compress xs
            compress ("ReflectX":xs)          = 'X' : compress xs
            compress ("ReflectY":xs)          = 'Y' : compress xs
            compress ("ResizableTall":xs)     = 'T' : compress xs
            compress ("Tall":xs)              = 'T' : compress xs
            compress (_:xs)                   = compress xs
            compress []                       = ""
