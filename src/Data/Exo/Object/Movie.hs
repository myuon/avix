module Data.Exo.Object.Movie where

import Control.Lens
import Data.Extensible
import Data.Exo.ExoFormat

-- |
-- @
-- type MovieR =
--   [ "_再生位置" >: Double
--   , "_再生速度" >: Double
--   , "_ループ再生" >: Bool
--   , "_アルファチャンネルを読み込む" >: Bool
--   , "file" >: FilePath
--   ]
-- @
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
    = unlinePairs $ toPairs
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

