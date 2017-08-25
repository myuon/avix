module Data.Exo.Records.Renderer where

import Control.Lens
import Data.Extensible
import Data.Exo.Types
import Data.Exo.ExoFormat
import qualified Data.Text.Lazy as T

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

type RendererR =
  [ "_X" >: Double
  , "_Y" >: Double
  , "_Z" >: Double
  , "_拡大率" >: Double
  , "_透明度" >: Double
  , "_回転" >: Double
  , "blend" >: BlendMode
  ]

newtype Renderer = Renderer { getParameter :: Record RendererR }
makeWrapped ''Renderer

instance ExoFormat Renderer where
  eformat n (Renderer r)
    = unlinePairs $ toPairs
    $ #__name @= "標準描画"
    <: #_X @= (r ^. #_X ^. to showt)
    <: #_Y @= (r ^. #_Y ^. to showt)
    <: #_Z @= (r ^. #_Z ^. to showt)
    <: #_拡大率 @= (r ^. #_拡大率 ^. to showt)
    <: #_透明度 @= (r ^. #_透明度 ^. to showt)
    <: #_回転 @= (r ^. #_回転 ^. to showt)
    <: #blend @= (r ^. #blend ^. from enum . to showt)
    <: emptyRecord

  def = Renderer
    $ #_X @= 0
    <: #_Y @= 0
    <: #_Z @= 0
    <: #_拡大率 @= 100
    <: #_透明度 @= 0
    <: #_回転 @= 0
    <: #blend @= Normal
    <: emptyRecord

