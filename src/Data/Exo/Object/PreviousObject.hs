module Data.Exo.Object.PreviousObject where

import Control.Lens
import Data.Extensible
import Data.Exo.ExoFormat

data PreviousObject = PreviousObject { getPreviousObject :: () }
makeWrapped ''PreviousObject

instance ExoFormat PreviousObject where
  eformat n _
    = unlinePairs $ toPairs
    $ #__name @= "直前オブジェクト"
    <: emptyRecord

  def = PreviousObject ()

