{-# LANGUAGE Rank2Types #-}
module Data.Exo.Types where

import Control.Lens
import Data.Word
import Data.List (lookup)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as T
import Numeric.Lens

data RGB = RGB Word8 Word8 Word8

_RGB :: Getter RGB T.Text
_RGB = to $ \case
  RGB r g b -> T.pack $ concat [r ^. re hex, g ^. re hex, b ^. re hex]

isoGraph :: (Eq a, Eq b) => [(a,b)] -> Iso' a b
isoGraph dic = iso (\b -> fromJust $ lookup b dic) (\b -> fromJust $ lookup b $ fmap (^. swapped) dic)

