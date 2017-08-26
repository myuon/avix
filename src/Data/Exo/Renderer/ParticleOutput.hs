module Data.Exo.Renderer.ParticleOutput where

import Control.Lens
import Data.Extensible
import Data.Exo.Types
import Data.Exo.ExoFormat

-- |
-- @
-- type ParticleOutputR =
--   [ "_X" >: Double
--   , "_Y" >: Double
--   , "_Z" >: Double
--   , "_出力頻度" >: Double
--   , "_出力速度" >: Double
--   , "_加速度" >: Double
--   , "_出力方向" >: Double
--   , "_拡散角度" >: Double
--   , "_透過率" >: Double
--   , "_透過速度" >: Double
--   , "_拡大率" >: Double
--   , "_拡大速度" >: Double
--   , "_回転角" >: Double
--   , "_回転速度" >: Double
--   , "_重力" >: Double
--   , "_生存時間" >: Double
--   , "_出力方向の基準を移動方向にする" >: Bool
--   , "_移動範囲の座標からランダムに出力" >: Bool
--   , "_3Dランダム回転" >: Bool
--   , "_終了点で全て消えるように調整する" >: Bool
--   , "blend" >: BlendMode
--   ]
-- @
type ParticleOutputR =
  [ "_X" >: Double
  , "_Y" >: Double
  , "_Z" >: Double
  , "_出力頻度" >: Double
  , "_出力速度" >: Double
  , "_加速度" >: Double
  , "_出力方向" >: Double
  , "_拡散角度" >: Double
  , "_透過率" >: Double
  , "_透過速度" >: Double
  , "_拡大率" >: Double
  , "_拡大速度" >: Double
  , "_回転角" >: Double
  , "_回転速度" >: Double
  , "_重力" >: Double
  , "_生存時間" >: Double
  , "_出力方向の基準を移動方向にする" >: Bool
  , "_移動範囲の座標からランダムに出力" >: Bool
  , "_3Dランダム回転" >: Bool
  , "_終了点で全て消えるように調整する" >: Bool
  , "blend" >: BlendMode
  ]

newtype ParticleOutput = ParticleOutput { getParticleOutput :: Record ParticleOutputR }
makeWrapped ''ParticleOutput

instance ExoFormat ParticleOutput where
  eformat n (ParticleOutput r)
    = unlinePairs $ toPairs
    $ #__name @= "パーティクル出力"
    <: #_X @= (r ^. #_X ^. to showt)
    <: #_Y @= (r ^. #_Y ^. to showt)
    <: #_Z @= (r ^. #_Z ^. to showt)
    <: #_出力頻度 @= (r ^. #_出力頻度 ^. to showt)
    <: #_出力速度 @= (r ^. #_出力速度 ^. to showt)
    <: #_加速度 @= (r ^. #_加速度 ^. to showt)
    <: #_出力方向 @= (r ^. #_出力方向 ^. to showt)
    <: #_拡散角度 @= (r ^. #_拡散角度 ^. to showt)
    <: #_透過率 @= (r ^. #_透過率 ^. to showt)
    <: #_透過速度 @= (r ^. #_透過速度 ^. to showt)
    <: #_拡大率 @= (r ^. #_拡大率 ^. to showt)
    <: #_拡大速度 @= (r ^. #_拡大速度 ^. to showt)
    <: #_回転角 @= (r ^. #_回転角 ^. to showt)
    <: #_回転速度 @= (r ^. #_回転速度 ^. to showt)
    <: #_重力 @= (r ^. #_重力 ^. to showt)
    <: #_生存時間 @= (r ^. #_生存時間 ^. to showt)
    <: #_出力方向の基準を移動方向にする @= (r ^. #_出力方向の基準を移動方向にする ^. to showBin)
    <: #_移動範囲の座標からランダムに出力 @= (r ^. #_移動範囲の座標からランダムに出力 ^. to showBin)
    <: #_3Dランダム回転 @= (r ^. #_3Dランダム回転 ^. to showBin)
    <: #_終了点で全て消えるように調整する @= (r ^. #_終了点で全て消えるように調整する ^. to showBin)
    <: #blend @= (r ^. #blend ^. from enum . to showt)
    <: emptyRecord

  def = ParticleOutput
    $ #_X @= 0
    <: #_Y @= 0
    <: #_Z @= 0
    <: #_出力頻度 @= 20
    <: #_出力速度 @= 400
    <: #_加速度 @= 0
    <: #_出力方向 @= 0
    <: #_拡散角度 @= 30
    <: #_透過率 @= 0
    <: #_透過速度 @= 0
    <: #_拡大率 @= 100
    <: #_拡大速度 @= 0
    <: #_回転角 @= 0
    <: #_回転速度 @= 0
    <: #_重力 @= 0
    <: #_生存時間 @= 0
    <: #_出力方向の基準を移動方向にする @= False
    <: #_移動範囲の座標からランダムに出力 @= False
    <: #_3Dランダム回転 @= False
    <: #_終了点で全て消えるように調整する @= True
    <: #blend @= Normal
    <: emptyRecord

