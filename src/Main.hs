module Main where

import Data.Extensible
import Data.Proxy
import Data.Word
import Data.Maybe
import Control.Lens hiding ((...))
import Numeric.Natural
import Numeric.Interval
import Numeric.Lens (hex)
import qualified Data.Text as T
import Linear.V2
import GHC.TypeLits

toPairs :: Forall (KeyValue KnownSymbol ((~) String)) xs => Record xs -> [(String, String)]
toPairs r = henumerateFor (Proxy @(KeyValue KnownSymbol ((~) String))) r (\mem -> (:) (symbolVal (proxyAssocKey mem) , r ^. itemAt mem)) []

unlinePairs :: [(String, String)] -> String
unlinePairs = unlines . fmap (\(k,v) -> stripUS k ++ "=" ++ v) where
  stripUS ('_' : k) = k
  stripUS k = k

class ExoFormat t where
  eformat :: Int -> t -> String
  def :: t

type ExeditR =
  [ "width" >: Int
  , "height" >: Int
  , "rate" >: Int
  , "scale" >: Double
  , "length" >: Int
  , "audio_rate" >: Int
  , "audio_ch" >: Int
  ]

newtype Exedit = Exedit { getExedit :: Record ExeditR }
makeWrapped ''Exedit

instance ExoFormat Exedit where
  eformat _ (Exedit r)
    = unlinePairs $ toPairs
    $ #width @= show (r ^. #width)
    <: #height @= show (r ^. #height)
    <: #rate @= show (r ^. #rate)
    <: #scale @= show (r ^. #scale)
    <: #length @= show (r ^. #length)
    <: #audio_rate @= show (r ^. #audio_rate)
    <: #audio_ch @= show (r ^. #audio_ch)
    <: emptyRecord

  def = Exedit $
    #width @= 1280
    <: #height @= 720
    <: #rate @= 30
    <: #scale @= 1
    <: #length @= 1
    <: #audio_rate @= 44100
    <: #audio_ch @= 2
    <: emptyRecord

_size :: Lens' Exedit (V2 Int)
_size = lens
  (\r -> V2 (r ^. _Wrapped . #width) (r ^. _Wrapped . #height))
  (\r v -> r & _Wrapped . #width .~ v ^. _x & _Wrapped . #height .~ v ^. _y)

data Resolution = VGA | HD | FullHD | Niconico | Custom (V2 Int)

_resolution :: Getter Resolution (V2 Int)
_resolution = to $ \case
  VGA -> V2 640 480
  HD -> V2 1280 720
  FullHD -> V2 1920 1080
  Niconico -> V2 960 540
  Custom v -> v

data RenderType = Standard

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
_blendMode = iso (\b -> fromJust $ lookup b dic) (\b -> fromJust $ lookup b $ fmap (^. swapped) dic) where
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

type ParameterR =
  [ "__name" >: RenderType
  , "_X" >: Double
  , "_Y" >: Double
  , "_Z" >: Double
  , "_拡大率" >: Double
  , "_透明度" >: Double
  , "_回転" >: Double
  , "blend" >: BlendMode
  ]

newtype Parameter = Parameter { getParameter :: Record ParameterR }
makeWrapped ''Parameter

instance ExoFormat Parameter where
  eformat _ (Parameter r)
    = unlinePairs $ toPairs
    $ #__name @= "標準描画"
    <: #_X @= show (r ^. #_X)
    <: #_Y @= show (r ^. #_Y)
    <: #_Z @= show (r ^. #_Z)
    <: #_拡大率 @= show (r ^. #_拡大率)
    <: #_透明度 @= show (r ^. #_透明度)
    <: #_回転 @= show (r ^. #_回転)
    <: #blend @= (show $ fromEnum $ r ^. #blend)
    <: emptyRecord

  def = Parameter
    $ #__name @= Standard
    <: #_X @= 0
    <: #_Y @= 0
    <: #_Z @= 0
    <: #_拡大率 @= 100
    <: #_透明度 @= 0
    <: #_回転 @= 0
    <: #blend @= Normal
    <: emptyRecord

data RGB = RGB Word8 Word8 Word8

_RGB :: Getter RGB String
_RGB = to $ \case
  RGB r g b -> r ^. re hex ++ g ^. re hex ++ b ^. re hex

type FigureR =
  [ "__name" >: String
  , "_サイズ" >: Int
  , "_縦横比" >: Double
  , "_ライン幅" >: Double
  , "_type" >: Int
  , "color" >: RGB
  , "name" >: String
  ]

newtype Figure = Figure { getFigure :: Record FigureR }
makeWrapped ''Figure

instance ExoFormat Figure where
  eformat _ (Figure r)
    = unlinePairs $ toPairs
    $ #__name @= r ^. #__name
    <: #_サイズ @= show (r ^. #_サイズ)
    <: #_縦横比 @= show (r ^. #_縦横比)
    <: #_ライン幅 @= show (r ^. #_ライン幅)
    <: #_type @= show (r ^. #_type)
    <: #color @= (r ^. #color ^. _RGB)
    <: #name @= r ^. #name
    <: emptyRecord

  def = Figure
    $ #__name @= "図形"
    <: #_サイズ @= 100
    <: #_縦横比 @= 0
    <: #_ライン幅 @= 4000
    <: #_type @= 1
    <: #color @= RGB 255 255 255
    <: #name @= ""
    <: emptyRecord

type TLObjectR =
  [ "start" >: Natural
  , "end" >: Natural
  , "layer" >: Int
--  , "overlay" >: Int これなあに？
  , "camera" >: Bool
  , "clipping" >: Bool
  , "figure" >: Figure
  , "parameter" >: Parameter
  ]

newtype TLObject = TLObject { getTLObject :: Record TLObjectR }
makeWrapped ''TLObject

showBin :: Bool -> String
showBin True = "1"
showBin False = "0"

showOpt :: Bool -> String
showOpt True = "1"
showOpt False = ""

instance ExoFormat TLObject where
  eformat n (TLObject r)
    = unlines $ (:) ("[" ++ show n ++ "]") $ fmap (\(k,v) -> fromPair k v) $ toPairs
    $ #start @= show (r ^. #start)
    <: #end @= show (r ^. #end)
    <: #layer @= show (r ^. #layer)
    <: #overlay @= "1"
    <: #camera @= showBin (r ^. #camera)
    <: #clipping @= showOpt (r ^. #clipping)
    <: #figure @= eformat n (r ^. #figure)
    <: #parameter @= eformat n (r ^. #parameter)
    <: emptyRecord

    where
      fromPair "clipping" v = v
      fromPair "figure" v = "[" ++ show n ++ ".0]\n" ++ v
      fromPair "parameter" v = "[" ++ show n ++ ".1]\n" ++ v
      fromPair k v = k ++ "=" ++ v

  def = TLObject
    $ #start @= 1
    <: #end @= 2
    <: #layer @= 1
    <: #camera @= False
    <: #clipping @= False
    <: #figure @= def
    <: #parameter @= def
    <: emptyRecord

_TLinterval :: Lens' TLObject (Interval Natural)
_TLinterval = lens
  (\r -> r ^. _Wrapped . #start ... r ^. _Wrapped . #end)
  (\r int -> r & _Wrapped . #start .~ inf int & _Wrapped . #end .~ sup int)

printExo :: Exedit -> IO ()
printExo ex = do
  putStrLn "[exedit]"
  putStrLn $ eformat 0 ex
  putStrLn $ eformat 0 (def @TLObject)

main :: IO ()
main = do
  printExo def

