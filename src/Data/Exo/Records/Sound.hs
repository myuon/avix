module Data.Exo.Records.Sound where

import Control.Lens
import Data.Extensible
import Data.Exo.ExoFormat
import Data.Text.Format (format)
import qualified Data.Text.Lazy as T

type SoundR =
  [ "_再生位置" >: Double
  , "_再生速度" >: Double
  , "_ループ再生" >: Bool
  , "_動画ファイルと連携" >: Bool
  , "file" >: FilePath
  ]

newtype Sound = Sound { getSound :: Record SoundR }
makeWrapped ''Sound

instance ExoFormat Sound where
  eformat n (Sound r)
    = T.append (format "[{}.0]\n" [n]) $ unlinePairs $ toPairs
    $ #__name @= "音声ファイル"
    <: #_再生位置 @= (r ^. #_再生位置 ^. to showt)
    <: #_再生速度 @= (r ^. #_再生速度 ^. to showt)
    <: #_ループ再生 @= (r ^. #_ループ再生 ^. to showBin)
    <: #_動画ファイルと連携 @= (r ^. #_動画ファイルと連携 ^. to showBin)
    <: #file @= (r ^. #file ^. to showt)
    <: emptyRecord

  def = Sound
    $ #_再生位置 @= 0
    <: #_再生速度 @= 100
    <: #_ループ再生 @= False
    <: #_動画ファイルと連携 @= False
    <: #file @= ""
    <: emptyRecord

