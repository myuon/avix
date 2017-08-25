module Data.Exo.Records.Image where

import Control.Lens
import Data.Extensible
import Data.Exo.ExoFormat
import Data.Text.Format (format)
import qualified Data.Text.Lazy as T

type ImageR =
  '[ "file" >: FilePath
  ]

newtype Image = Image { getImage :: Record ImageR }
makeWrapped ''Image

instance ExoFormat Image where
  eformat n (Image r)
    = T.append (format "[{}.0]\n" [n]) $ unlinePairs $ toPairs
    $ #file @= ""
    <: emptyRecord

  def = Image
    $ #file @= ""
    <: emptyRecord

