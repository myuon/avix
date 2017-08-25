module Data.Exo.Records.Exedit where

import Control.Lens
import Data.Extensible
import Data.Exo.ExoFormat
import qualified Data.Text.Lazy as T
import Linear.V2


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

