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

data BlendMode
  = Normal         -- 通常
  | Addition       -- 加算
  | Subtraction    -- 減算
  | Multiply       -- 乗算
  | Screen         -- スクリーン
  | Overlay        -- オーバーレイ
  | Lighten        -- 比較(明)
  | Darken         -- 比較(暗)
  | Luminosity     -- 輝度
  | ColorDistance  -- 色差
  | Shadow         -- 陰影
  | Brightness     -- 明暗
  | Difference     -- 差分
  deriving (Eq, Enum, Show)

_blendMode :: Iso' BlendMode T.Text
_blendMode = isoGraph dic where
  dic =
    [ (Normal, "通常")
    , (Addition, "加算")
    , (Subtraction, "減算")
    , (Multiply, "乗算")
    , (Screen, "スクリーン")
    , (Overlay, "オーバーレイ")
    , (Lighten, "比較(明)")
    , (Darken, "比較(暗)")
    , (Luminosity, "輝度")
    , (ColorDistance, "色差")
    , (Shadow, "陰影")
    , (Brightness, "明暗")
    , (Difference, "差分")
    ]

