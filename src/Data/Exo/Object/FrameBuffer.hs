module Data.Exo.Object.FrameBuffer where

import Control.Lens
import Data.Extensible
import Data.Exo.ExoFormat

-- |
-- @
-- type FrameBufferR =
--   '[ "_フレームバッファをクリア" >: Bool
--   ]
-- @
type FrameBufferR =
  '[ "_フレームバッファをクリア" >: Bool
  ]

newtype FrameBuffer = FrameBuffer { getFrameBuffer :: Record FrameBufferR }
makeWrapped ''FrameBuffer

instance ExoFormat FrameBuffer where
  eformat n (FrameBuffer r)
    = unlinePairs $ toPairs
    $ #__name @= "フレームバッファ"
    <: #_フレームバッファをクリア @= (r ^. #_フレームバッファをクリア ^. to showBin)
    <: emptyRecord

  def = FrameBuffer
    $ #_フレームバッファをクリア @= False
    <: emptyRecord

