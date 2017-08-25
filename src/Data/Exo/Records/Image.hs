module Data.Exo.Records.Image where

import Control.Lens
import Data.Extensible
import Data.Exo.ExoFormat

type ImageR =
  '[ "file" >: FilePath
  ]

newtype Image = Image { getImage :: Record ImageR }
makeWrapped ''Image

instance ExoFormat Image where
  eformat n (Image r)
    = unlinePairs $ toPairs
    $ #__name @= "画像ファイル"
    <: #file @= ""
    <: emptyRecord

  def = Image
    $ #file @= ""
    <: emptyRecord

