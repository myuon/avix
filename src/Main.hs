module Main where

import Data.Extensible
import Data.Extensible.HList
import Data.Proxy
import Data.Type.Equality
import Control.Lens
import Linear.V2
import GHC.TypeLits

toPairs :: Forall (KeyValue KnownSymbol ((~) String)) xs => Record xs -> [(String, String)]
toPairs r = henumerateFor (Proxy @(KeyValue KnownSymbol ((~) String))) r (\mem -> (:) (symbolVal (proxyAssocKey mem) , r ^. itemAt mem)) []

class ExoFormat t where
  eformat :: t -> String
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
  eformat (Exedit r)
    = unlines $ fmap (\(k,v) -> k ++ "=" ++ v) $ toPairs
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

type TLObjectR =
  [ "start" >: Int
  , "end" >: Int
  , "layer" >: Int
  , "overlay" >: Int
  , "camera" >: Bool
  , "clipping" >: Maybe Int
  , "parameter" >: Parameter
  , "figure" >: Figure
  ]

data RenderType = Standard

type Parameter = Record
  [ "_name" >: RenderType
  , "X" >: Double
  , "Y" >: Double
  , "Z" >: Double
  , "拡大率" >: Double
  , "透明度" >: Double
  , "回転" >: Double
  , "blend" >: Int
  ]

data Color = Color Int Int Int

type Figure = Record
  [ "_name" >: String
  , "サイズ" >: Int
  , "縦横比" >: Double
  , "ライン幅" >: Int
  , "type" >: Int
  , "color" >: Color
  , "name" >: String
  ]

newtype TLObject = TLObject { getTLObject :: Record TLObjectR }
makeWrapped ''TLObject

printExo :: Exedit -> IO ()
printExo ex = do
  putStrLn "[exedit]"
  putStrLn $ eformat ex

main :: IO ()
main = do
  printExo def

