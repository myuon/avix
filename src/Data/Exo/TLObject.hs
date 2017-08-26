module Data.Exo.TLObject where

import Data.Exo.ExoFormat
import Data.Exo.Object
import Data.Exo.Renderer

import Control.Lens hiding ((...))
import Data.Extensible
import Data.Text.Format (format)
import qualified Data.Text.Lazy as T
import Numeric.Interval
import Numeric.Natural
import Linear.V2

data Resolution = VGA | HD | FullHD | Niconico | Custom (V2 Int)

-- |
-- >>> VGA ^. _resolution
-- V2 640 480
-- >>> HD ^. _resolution
-- V2 1280 720
-- >>> FullHD ^. _resolution
-- V2 1920 1080
-- >>> Niconico ^. _resolution
-- V2 960 540
-- >>> (Custom v) ^. _resolution
-- v
_resolution :: Getter Resolution (V2 Int)
_resolution = to $ \case
  VGA -> V2 640 480
  HD -> V2 1280 720
  FullHD -> V2 1920 1080
  Niconico -> V2 960 540
  Custom v -> v


type TLObjectR =
  [ "start" >: Natural
  , "end" >: Natural
  , "layer" >: Int
--  , "overlay" >: Int これなあに？
  , "camera" >: Bool
  , "clipping" >: Bool
  , "object" >: Variant
    [ "movie" >: Movie
    , "sound" >: Sound
    , "figure" >: Figure
    ]
  , "renderer" >: StdRenderer
  ]

-- |
-- === Example
--
-- > def & #object .~ embed (#movie @= def)
newtype TLObject = TLObject { getTLObject :: Record TLObjectR }
makeWrapped ''TLObject

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
      mat :: RecordOf (Match Identity T.Text) ["movie" >: Movie, "sound" >: Sound, "figure" >: Figure]
      mat = #movie @= eformat n
        <: #sound @= eformat n
        <: #figure @= eformat n
        <: nil
      
      fromPair :: T.Text -> T.Text -> T.Text
      fromPair "clipping" v | v == "" = ""
      fromPair "object" v = format "[{}.0]\n{}" [showt n, v]
      fromPair "renderer" v = format "[{}.1]\n{}" [showt n, v]
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

-- |
-- 'Interval' of TLObject: [start, end]
--
-- @
-- 'TLObject' r ^. _TLinterval == (r ^. #start ... r ^. #end)
-- @
_TLinterval :: Lens' TLObject (Interval Natural)
_TLinterval = lens
  (\r -> (r ^. _Wrapped . #start) ... (r ^. _Wrapped . #end))
  (\r int -> r & _Wrapped . #start .~ inf int & _Wrapped . #end .~ sup int)

