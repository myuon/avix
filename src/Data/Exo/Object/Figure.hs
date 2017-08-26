module Data.Exo.Object.Figure where

import Control.Lens
import Data.Extensible
import Data.Exo.ExoFormat
import Data.Exo.Types
import qualified Data.Text.Lazy as T

-- |
-- @
-- type FigureR =
--   [ "_サイズ" >: Int
--   , "_縦横比" >: Double
--   , "_ライン幅" >: Double
--   , "_type" >: Int
--   , "color" >: RGB
--   , "name" >: T.Text
--   ]
-- @
type FigureR =
  [ "_サイズ" >: Int
  , "_縦横比" >: Double
  , "_ライン幅" >: Double
  , "_type" >: Int
  , "color" >: RGB
  , "name" >: T.Text
  ]

newtype Figure = Figure { getFigure :: Record FigureR }
makeWrapped ''Figure

instance ExoFormat Figure where
  eformat n (Figure r)
    = unlinePairs $ toPairs
    $ #__name @= "図形"
    <: #_サイズ @= (r ^. #_サイズ ^. to showt)
    <: #_縦横比 @= (r ^. #_縦横比 ^. to showt)
    <: #_ライン幅 @= (r ^. #_ライン幅 ^. to showt)
    <: #_type @= (r ^. #_type ^. to showt)
    <: #color @= (r ^. #color ^. _RGB)
    <: #name @= r ^. #name
    <: emptyRecord

  def = Figure
    $ #_サイズ @= 100
    <: #_縦横比 @= 0
    <: #_ライン幅 @= 4000
    <: #_type @= 1
    <: #color @= RGB 255 255 255
    <: #name @= ""
    <: emptyRecord

