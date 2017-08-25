{-# LANGUAGE MagicHash #-}
module Data.Exo.Records.Scene where

import Control.Lens
import Data.Extensible
import Data.Exo.ExoFormat
import GHC.OverloadedLabels
import GHC.Prim

type SceneR =
  [ "_再生位置" >: Int
  , "_再生速度" >: Double
  , "_ループ再生" >: Bool
  , "(null)" >: Int
  ]

newtype Scene = Scene { getScene :: Record SceneR }
makeWrapped ''Scene

instance ExoFormat Scene where
  eformat n (Scene r)
    = unlinePairs $ toPairs
    $ #__name @= "シーン"
    <: #_再生位置 @= (r ^. #_再生位置 ^. to showt)
    <: #_再生速度 @= (r ^. #_再生速度 ^. to showt)
    <: #_ループ再生 @= (r ^. #_ループ再生 ^. to showt)
    <: (fromLabel @"(null)" proxy#) @= (r ^. (fromLabel @"(null)" proxy#) ^. to showt)
    <: emptyRecord

  def = Scene
    $ #_再生位置 @= 1
    <: #_再生速度 @= 100
    <: #_ループ再生 @= False
    <: (fromLabel @"(null)" proxy#) @= 1
    <: emptyRecord

_scene :: Lens' Scene Int
_scene = _Wrapped . (fromLabel @"(null)" proxy#)

