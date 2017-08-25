{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import Data.Extensible
import Data.Proxy
import Data.Word
import Data.Maybe
import Data.Text.Format (format)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Control.Lens hiding ((...))
import Numeric.Natural
import Numeric.Interval
import Numeric.Lens (hex)
import Linear.V2
import GHC.TypeLits

toPairs :: Forall (KeyValue KnownSymbol ((~) T.Text)) xs => Record xs -> [(T.Text, T.Text)]
toPairs r = henumerateFor (Proxy @(KeyValue KnownSymbol ((~) T.Text))) r (\mem -> (:) (T.pack $ symbolVal (proxyAssocKey mem) , r ^. itemAt mem)) []

unlinePairs :: [(T.Text, T.Text)] -> T.Text
unlinePairs = T.unlines . fmap (\(k,v) -> T.concat [stripUS k, "=", v]) where
  stripUS (T.uncons -> Just ('_', k)) = k
  stripUS k = k

showt :: (Show a) => a -> T.Text
showt = T.pack . show

showBin :: Bool -> T.Text
showBin True = "1"
showBin False = "0"

showOpt :: Bool -> T.Text
showOpt True = "1"
showOpt False = ""

class ExoFormat t where
  eformat :: Int -> t -> T.Text
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
    = T.append "[exedit]\n" $ unlinePairs $ toPairs
    $ #width @= (r ^. #width ^. to showt)
    <: #height @= (r ^. #height ^. to showt)
    <: #rate @= (r ^. #rate ^. to showt)
    <: #scale @= (r ^. #scale ^. to showt)
    <: #length @= (r ^. #length ^. to showt)
    <: #audio_rate @= (r ^. #audio_rate ^. to showt)
    <: #audio_ch @= (r ^. #audio_ch ^. to showt)
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

data RGB = RGB Word8 Word8 Word8

_RGB :: Getter RGB T.Text
_RGB = to $ \case
  RGB r g b -> T.pack $ concat [r ^. re hex, g ^. re hex, b ^. re hex]


type MovieR =
  [ "_再生位置" >: Double
  , "_再生速度" >: Double
  , "_ループ再生" >: Bool
  , "_アルファチャンネルを読み込む" >: Bool
  , "file" >: FilePath
  ]

newtype Movie = Movie { getMovie :: Record MovieR }
makeWrapped ''Movie

instance ExoFormat Movie where
  eformat n (Movie r)
    = T.append (format "[{}.0]\n" [n]) $ unlinePairs $ toPairs
    $ #__name @= "動画ファイル"
    <: #_再生位置 @= (r ^. #_再生位置 ^. to showt)
    <: #_再生速度 @= (r ^. #_再生速度 ^. to showt)
    <: #_ループ再生 @= (r ^. #_ループ再生 ^. to showBin)
    <: #_アルファチャンネルを読み込む @= (r ^. #_アルファチャンネルを読み込む ^. to showBin)
    <: #file @= (r ^. #file ^. to showt)
    <: emptyRecord
  
  def = Movie
    $ #_再生位置 @= 0
    <: #_再生速度 @= 100
    <: #_ループ再生 @= False
    <: #_アルファチャンネルを読み込む @= False
    <: #file @= ""
    <: emptyRecord


type FigureR =
  [ "_サイズ" >: Int
  , "_縦横比" >: Double
  , "_ライン幅" >: Double
  , "_type" >: Int
  , "color" >: RGB
  , "name" >: T.Text
  ]

newtype Figure = Figure { getFigure :: Record FigureR }
makeWrapped ''Figure

instance ExoFormat Figure where
  eformat n (Figure r)
    = T.append (format "[{}.0]\n" [n]) $ unlinePairs $ toPairs
    $ #__name @= "図形"
    <: #_サイズ @= (r ^. #_サイズ ^. to showt)
    <: #_縦横比 @= (r ^. #_縦横比 ^. to showt)
    <: #_ライン幅 @= (r ^. #_ライン幅 ^. to showt)
    <: #_type @= (r ^. #_type ^. to showt)
    <: #color @= (r ^. #color ^. _RGB)
    <: #name @= r ^. #name
    <: emptyRecord

  def = Figure
    $ #_サイズ @= 100
    <: #_縦横比 @= 0
    <: #_ライン幅 @= 4000
    <: #_type @= 1
    <: #color @= RGB 255 255 255
    <: #name @= ""
    <: emptyRecord

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
    = T.append (format "[{}.1]\n" [n]) $ unlinePairs $ toPairs
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

type TLObjectR =
  [ "start" >: Natural
  , "end" >: Natural
  , "layer" >: Int
--  , "overlay" >: Int これなあに？
  , "camera" >: Bool
  , "clipping" >: Bool
  , "object" >: Variant
    [ "movie" >: Movie
--    , "sound" >: Record '[]
    , "figure" >: Figure
    ]
  , "renderer" >: Renderer
  ]

newtype TLObject = TLObject { getTLObject :: Record TLObjectR }
makeWrapped ''TLObject

class ExoFormatAssoc t where
  eformatAssoc :: Int -> AssocValue t -> T.Text
  
instance ExoFormat v => ExoFormatAssoc (k >: v) where
  eformatAssoc = eformat

instance ExoFormat TLObject where
  eformat n (TLObject r)
    = T.append (format "[{}]\n" [n]) $ T.unlines $ fmap (uncurry fromPair) $ toPairs
    $ #start @= (r ^. #start ^. to showt)
    <: #end @= (r ^. #end ^. to showt)
    <: #layer @= (r ^. #layer ^. to showt)
    <: #overlay @= "1"
    <: #camera @= (r ^. #camera ^. to showBin)
    <: #clipping @= (r ^. #clipping ^. to showOpt)
    <: #object @= matchField mat (r ^. #object)
    <: #renderer @= eformat n (r ^. #renderer)
    <: emptyRecord

    where
      mat :: RecordOf (Match Identity T.Text) ["movie" >: Movie, "figure" >: Figure]
      mat = #movie @= eformat n
        <: #figure @= eformat n
        <: nil
      
      fromPair :: T.Text -> T.Text -> T.Text
      fromPair "clipping" v | v == "" = ""
      fromPair "figure" v = v
      fromPair "parameter" v = v
      fromPair k v = format "{}={}" [k, v]

  def = TLObject
    $ #start @= 1
    <: #end @= 2
    <: #layer @= 1
    <: #camera @= False
    <: #clipping @= False
    <: #object @= undefined
    <: #renderer @= def
    <: emptyRecord

_TLinterval :: Lens' TLObject (Interval Natural)
_TLinterval = lens
  (\r -> (r ^. _Wrapped . #start) ... (r ^. _Wrapped . #end))
  (\r int -> r & _Wrapped . #start .~ inf int & _Wrapped . #end .~ sup int)

getAssocValue :: Field h kv -> h (AssocValue kv)
getAssocValue fh = getField fh

printExo :: Exedit -> IO ()
printExo ex = do
  T.putStrLn $ eformat 0 $ ex & _Wrapped . #length .~ 10
  T.putStrLn $ eformat 0 $ (def @TLObject) & _Wrapped . #object .~ embed (#figure @= def @Figure)

main :: IO ()
main = do
  printExo def

