module Data.Exo.Records.Text where

import Control.Lens
import Data.Extensible
import Data.Exo.Types
import Data.Exo.ExoFormat
import Data.Text.Format (format)
import qualified Data.Text.Lazy as T

data RenderType = RenderNormal | Shaded | ShadedLight | Bordered | BorderedThin
  deriving (Eq, Enum, Show)

_renderType :: Iso' RenderType T.Text
_renderType = isoGraph dic where
  dic =
    [ (RenderNormal, "標準文字")
    , (Shaded, "影付き文字")
    , (ShadedLight, "影付き文字(薄)")
    , (Bordered, "縁取り文字")
    , (BorderedThin, "縁取り文字(細)")
    ]

data Vertical = VTop | VMiddle | VBottom deriving (Eq, Enum, Show)
data Horizontal = HLeft | HCenter | HRight deriving (Eq, Enum, Show)

type TextAlign = Record
  [ "isVertical" >: Bool
  , "vertical" >: Vertical
  , "horizontal" >: Horizontal
  ]

_textAlign :: Getter TextAlign Int
_textAlign = to $ \ta -> if not (ta ^. #isVertical)
  then 3 * (ta ^. #vertical ^. from enum) + (ta ^. #horizontal ^. from enum)
  else 9 + 3 * (2 - ta ^. #horizontal ^. from enum) + (ta ^. #vertical ^. from enum)

type TextR =
  [ "_サイズ" >: Int
  , "_表示速度" >: Int
  , "_文字毎に個別オブジェクト" >: Bool
  , "_移動座標上に表示する" >: Bool
  , "_自動スクロール" >: Bool
  , "_B" >: Bool  -- 太字
  , "_I" >: Bool  -- イタリック
  , "_type" >: RenderType  -- 文字描画モード
  , "autoadjust" >: Bool  -- オブジェクトの長さを自動調節
  , "soft" >: Bool  -- 滑らかにする
  , "monospace" >: Bool  -- 等間隔モード
  , "align" >: TextAlign  -- テキスト寄せ
  , "spacing_x" >: Int  -- 字間
  , "spacing_y" >: Int  -- 行間
  , "precision" >: Bool -- 高精度モード
  , "color" >: RGB  -- 文字色の設定
  , "color2" >: RGB  -- 影・縁色の設定
  , "font" >: String  -- フォント
  , "text" >: String
  ]

newtype Text = Text { getText :: Record TextR }
makeWrapped ''Text

_オブジェクトの長さを自動調節 :: Lens' Text Bool
_オブジェクトの長さを自動調節 = _Wrapped . #autoadjust

_fontColor :: Lens' Text RGB
_fontColor = _Wrapped . #color

_borderColor :: Lens' Text RGB
_borderColor = _Wrapped . #color2

_字間 :: Lens' Text Int
_字間 = _Wrapped . #spacing_x

_行間 :: Lens' Text Int
_行間 = _Wrapped . #spacing_y

_等間隔モード :: Lens' Text Bool
_等間隔モード = _Wrapped . #monospace

_高精度モード :: Lens' Text Bool
_高精度モード = _Wrapped . #precision

_滑らかにする :: Lens' Text Bool
_滑らかにする = _Wrapped . #soft

instance ExoFormat Text where
  eformat n (Text r)
    = T.append (format "[{}.0]\n" [n]) $ unlinePairs $ toPairs
    $ #_サイズ @= (r ^. #_サイズ ^. to showt)
    <: #_表示速度 @= (r ^. #_表示速度 ^. to showt)
    <: #_文字毎に個別オブジェクト @= (r ^. #_文字毎に個別オブジェクト ^. to showBin)
    <: #_移動座標上に表示する @= (r ^. #_移動座標上に表示する ^. to showBin)
    <: #_自動スクロール @= (r ^. #_自動スクロール ^. to showBin)
    <: #_B @= (r ^. #_B ^. to showBin)
    <: #_I @= (r ^. #_I ^. to showBin)
    <: #_type @= (r ^. #_type ^. from enum ^. to showt)
    <: #autoadjust @= (r ^. #autoadjust ^. to showBin)
    <: #soft @= (r ^. #soft ^. to showBin)
    <: #monospace @= (r ^. #monospace ^. to showBin)
    <: #align @= (r ^. #align ^. _textAlign ^. to showt)
    <: #spacing_x @= (r ^. #spacing_x ^. to showt)
    <: #spacing_y @= (r ^. #spacing_y ^. to showt)
    <: #precision @= (r ^. #precision ^. to showBin)
    <: #color @= (r ^. #color ^. _RGB)
    <: #color2 @= (r ^. #color2 ^. _RGB)
    <: #font @= (r ^. #font ^. to showt)
    <: #text @= ""
    <: emptyRecord

  def = Text
    $ #_サイズ @= 34
    <: #_表示速度 @= 0
    <: #_文字毎に個別オブジェクト @= False
    <: #_移動座標上に表示する @= False
    <: #_自動スクロール @= False
    <: #_B @= False
    <: #_I @= False
    <: #_type @= RenderNormal
    <: #autoadjust @= False
    <: #soft @= True
    <: #monospace @= False
    <: #align @=
         (#isVertical @= False
         <: #vertical @= VTop
         <: #horizontal @= HLeft
         <: emptyRecord)
    <: #spacing_x @= 0
    <: #spacing_y @= 0
    <: #precision @= True
    <: #color @= RGB 255 255 255
    <: #color2 @= RGB 0 0 0
    <: #font @= "MS UI Gothic"
    <: #text @= ""
    <: emptyRecord

