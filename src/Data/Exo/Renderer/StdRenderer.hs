module Data.Exo.Renderer.StdRenderer where

import Control.Lens
import Data.Extensible
import Data.Exo.Types
import Data.Exo.ExoFormat

type StdRendererR =
  [ "_X" >: Double
  , "_Y" >: Double
  , "_Z" >: Double
  , "_拡大率" >: Double
  , "_透明度" >: Double
  , "_回転" >: Double
  , "blend" >: BlendMode
  ]

newtype StdRenderer = StdRenderer { getStdRenderer :: Record StdRendererR }
makeWrapped ''StdRenderer

instance ExoFormat StdRenderer where
  eformat n (StdRenderer r)
    = unlinePairs $ toPairs
    $ #__name @= "標準描画"
    <: #_X @= (r ^. #_X ^. to showt)
    <: #_Y @= (r ^. #_Y ^. to showt)
    <: #_Z @= (r ^. #_Z ^. to showt)
    <: #_拡大率 @= (r ^. #_拡大率 ^. to showt)
    <: #_透明度 @= (r ^. #_透明度 ^. to showt)
    <: #_回転 @= (r ^. #_回転 ^. to showt)
    <: #blend @= (r ^. #blend ^. from enum . to showt)
    <: emptyRecord

  def = StdRenderer
    $ #_X @= 0
    <: #_Y @= 0
    <: #_Z @= 0
    <: #_拡大率 @= 100
    <: #_透明度 @= 0
    <: #_回転 @= 0
    <: #blend @= Normal
    <: emptyRecord

