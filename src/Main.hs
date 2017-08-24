{-# LANGUAGE UndecidableInstances #-}
module Main where

import Bookkeeper
import Bookkeeper.Internal (Book')
import Bookkeeper.Lens
import Bookkeeper.JSON
import Data.Aeson
import Data.Proxy
import Control.Lens
import Linear.V2
import GHC.TypeLits
import Internal

class ExoFormat t where
  eformat :: t -> String
  def :: t

type BExedit =
  [ "width" :=> Int
  , "height" :=> Int
  , "rate" :=> Int
  , "scale" :=> Double
  , "length" :=> Int
  , "audio_rate" :=> Int
  , "audio_ch" :=> Int
  ]

newtype Exedit = Exedit { getExedit :: Book BExedit }
makeWrapped ''Exedit

instance ExoFormat Exedit where
  eformat (Exedit book)
    = unlines $ fmap (\(k,v) -> k ++ "=" ++ v) $ bookPairs $ emptyBook
    & #width =: (book ^. #width) ^. to show
    & #height =: (book ^. #height) ^. to show
    & #rate =: (book ^. #rate) ^. to show
    & #scale =: (book ^. #scale) ^. to show
    & #length =: (book ^. #length) ^. to show
    & #audio_rate =: (book ^. #audio_rate) ^. to show
    & #audio_ch =: (book ^. #audio_ch) ^. to show

  def = Exedit $ emptyBook
    & #width =: 1280
    & #height =: 720
    & #rate =: 30
    & #scale =: 1
    & #length =: 1
    & #audio_rate =: 44100
    & #audio_ch =: 2

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

type BTLObject =
  [ "start" :=> Int
  , "end" :=> Int
  , "layer" :=> Int
  , "overlay" :=> Int
  , "camera" :=> Bool
  , "clipping" :=> Maybe Int
  , "parameter" :=> Parameter
  , "figure" :=> Figure
  ]

data RenderType = Standard

type Parameter = Book
  [ "_name" :=> RenderType
  , "X" :=> Double
  , "Y" :=> Double
  , "Z" :=> Double
  , "拡大率" :=> Double
  , "透明度" :=> Double
  , "回転" :=> Double
  , "blend" :=> Int
  ]

data Color = Color Int Int Int

type Figure = Book
  [ "_name" :=> String
  , "サイズ" :=> Int
  , "縦横比" :=> Double
  , "ライン幅" :=> Int
  , "type" :=> Int
  , "color" :=> Color
  , "name" :=> String
  ]

newtype TLObject = TLObject { getTLObject :: Book BTLObject }
makeWrapped ''TLObject

printExo :: Exedit -> IO ()
printExo ex = do
  putStrLn "[exedit]"
  putStrLn $ eformat ex

main :: IO ()
main = do
  printExo def

