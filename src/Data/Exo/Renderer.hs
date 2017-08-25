module Data.Exo.Renderer where

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

type StdRendererR =
  [ "_X" >: Double
  , "_Y" >: Double
  , "_Z" >: Double
  , "_拡大率" >: Double
  , "_透明度" >: Double
  , "_回転" >: Double
  , "blend" >: BlendMode
  ]

newtype StdRenderer = StdRenderer { getStdRenderer :: Record StdRendererR }
makeWrapped ''StdRenderer

instance ExoFormat StdRenderer where
  eformat n (StdRenderer r)
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

  def = StdRenderer
    $ #_X @= 0
    <: #_Y @= 0
    <: #_Z @= 0
    <: #_拡大率 @= 100
    <: #_透明度 @= 0
    <: #_回転 @= 0
    <: #blend @= Normal
    <: emptyRecord


type ExtRendererR =
  [ "_X" >: Double
  , "_Y" >: Double
  , "_Z" >: Double
  , "_拡大率" >: Double
  , "_透明度" >: Double
  , "_縦横比" >: Double
  , "_X軸回転" >: Double
  , "_Y軸回転" >: Double
  , "_Z軸回転" >: Double
  , "_中心X" >: Double
  , "_中心Y" >: Double
  , "_中心Z" >: Double
  , "_裏面を表示しない" >: Bool
  , "blend" >: BlendMode
  ]

newtype ExtRenderer = ExtRenderer { getExtRenderer :: Record ExtRendererR }
makeWrapped ''ExtRenderer

instance ExoFormat ExtRenderer where
  eformat n (ExtRenderer r)
    = unlinePairs $ toPairs
    $ #__name @= "拡張描画"
    <: #_X @= (r ^. #_X ^. to showt)
    <: #_Y @= (r ^. #_Y ^. to showt)
    <: #_Z @= (r ^. #_Z ^. to showt)
    <: #_拡大率 @= (r ^. #_拡大率 ^. to showt)
    <: #_透明度 @= (r ^. #_透明度 ^. to showt)
    <: #_縦横比 @= (r ^. #_縦横比 ^. to showt)
    <: #_X軸回転 @= (r ^. #_X軸回転 ^. to showt)
    <: #_Y軸回転 @= (r ^. #_Y軸回転 ^. to showt)
    <: #_Z軸回転 @= (r ^. #_Z軸回転 ^. to showt)
    <: #_中心X @= (r ^. #_中心X ^. to showt)
    <: #_中心Y @= (r ^. #_中心Y ^. to showt)
    <: #_中心Z @= (r ^. #_中心Z ^. to showt)
    <: #_裏面を表示しない @= (r ^. #_裏面を表示しない ^. to showBin)
    <: #blend @= (r ^. #blend ^. _blendMode)
    <: emptyRecord

  def = ExtRenderer
    $ #_X @= 0
    <: #_Y @= 0
    <: #_Z @= 0
    <: #_拡大率 @= 100
    <: #_透明度 @= 0
    <: #_縦横比 @= 0
    <: #_X軸回転 @= 0
    <: #_Y軸回転 @= 0
    <: #_Z軸回転 @= 0
    <: #_中心X @= 0
    <: #_中心Y @= 0
    <: #_中心Z @= 0
    <: #_裏面を表示しない @= False
    <: #blend @= Normal
    <: emptyRecord

