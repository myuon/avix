module Data.Exo.Object.SoundWave where

import Control.Lens
import Data.Extensible
import Data.Exo.Types
import Data.Exo.ExoFormat

data SoundWaveType = Type1 | Type2 | Type3 | Type4 | Type5
  deriving (Eq, Enum, Show)

type SoundWaveR =
  [ "_横幅" >: Int
  , "_高さ" >: Int
  , "_音量" >: Double
  , "_再生位置" >: Double
  , "_編集全体の音声を元にする" >: Bool
  , "file" >: FilePath
  , "_type" >: SoundWaveType
  , "mode" >: Bool  -- 波形タイプ(0/1) ← チェックボックスにはしない理由があるの？
  , "res_w" >: Int  -- 横解像度
  , "res_h" >: Int  -- 縦解像度
  , "pad_w" >: Int  -- 横スペース(%)
  , "pad_h" >: Int  -- 縦スペース(%)
  , "color" >: RGB  -- 波形の色
--  , "sample_n" >: _ ← なにこれ
  , "mirror" >: Int  -- ミラー
  ]

newtype SoundWave = SoundWave { getSoundWave :: Record SoundWaveR }
makeWrapped ''SoundWave

instance ExoFormat SoundWave where
  eformat n (SoundWave r)
    = unlinePairs $ toPairs
    $ #__name @= "音声波形表示"
    <: #_横幅 @= (r ^. #_横幅 ^. to showt)
    <: #_高さ @= (r ^. #_高さ ^. to showt)
    <: #_音量 @= (r ^. #_音量 ^. to showt)
    <: #_再生位置 @= (r ^. #_再生位置 ^. to showt)
    <: #_編集全体の音声を元にする @= (r ^. #_編集全体の音声を元にする ^. to showBin)
    <: #file @= (r ^. #file ^. to showt)
    <: #_type @= (r ^. #_type ^. from enum ^. to showt)
    <: #mode @= (r ^. #mode ^. to showBin)
    <: #res_w @= (r ^. #res_w ^. to showt)
    <: #res_h @= (r ^. #res_h ^. to showt)
    <: #pad_w @= (r ^. #pad_w ^. to showt)
    <: #pad_h @= (r ^. #pad_h ^. to showt)
    <: #color @= (r ^. #color ^. _RGB)
    <: #sample_n @= "0"
    <: #mirror @= (r ^. #mirror ^. to showt)
    <: emptyRecord

  def = SoundWave
    $ #_横幅 @= 640
    <: #_高さ @= 240
    <: #_音量 @= 100
    <: #_再生位置 @= 0
    <: #_編集全体の音声を元にする @= True
    <: #file @= ""
    <: #_type @= Type1
    <: #mode @= False
    <: #res_w @= 0
    <: #res_h @= 4096
    <: #pad_w @= 0
    <: #pad_h @= 0
    <: #color @= RGB 255 255 255
    <: #mirror @= 0
    <: emptyRecord

