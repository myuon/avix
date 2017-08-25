module Data.Exo.Object.CustomObject where

import Control.Lens
import Data.Extensible
import Data.Exo.ExoFormat
import Data.Text.Format (format)
import qualified Data.Text.Lazy as T

type Track = Variant
  [ "no_move" >: Double
  , "move" >: Record
    [ "start" >: Double
    , "end" >: Double
    , "mode" >: Int
    ]
  ]

_track :: Getter Track T.Text
_track = to $ matchField go where
  go :: RecordOf (Match Identity T.Text)
        [ "no_move" >: Double
        , "move" >: Record [ "start" >: Double, "end" >: Double, "mode" >: Int ]
        ]
  go
    = #no_move @= showt
    <: #move @= (\r -> format "{},{},{}" [ r ^. #start ^. to showt, r ^. #end ^. to showt, r ^. #mode ^. to showt ])
    <: nil

-- 読み込まれたプラグインを調べる方法はある？
-- AviUtl pluginファイルの読み込み順とかが分かればあるいは…
type CustomObjectR =
  [ "track0" >: Track
  , "track1" >: Track
  , "track2" >: Track
  , "track3" >: Track
--  , "check0" >: Double <- ??
  , "_type" >: Int  -- カスタムオブジェクト
--  , "filter" >: Bool <- ??
--  , "name" >: String <- ??
  , "param" >: T.Text
  ]

data CustomObject = CustomObject { getCustomObject :: Record CustomObjectR }
makeWrapped ''CustomObject

instance ExoFormat CustomObject where
  eformat n (CustomObject r)
    = unlinePairs $ toPairs
    $ #__name @= "カスタムオブジェクト"
    <: #track0 @= (r ^. #track0 ^. _track)
    <: #track1 @= (r ^. #track1 ^. _track)
    <: #track2 @= (r ^. #track2 ^. _track)
    <: #track3 @= (r ^. #track3 ^. _track)
    <: #check0 @= "0"
    <: #_type @= (r ^. #_type ^. to showt)
    <: #filter @= "0"
    <: #name @= ""
    <: #param @= (r ^. #param)
    <: emptyRecord

  def = CustomObject
    $ #track0 @= embed (#no_move @= (0 :: Double))
    <: #track1 @= embed (#no_move @= (0 :: Double))
    <: #track2 @= embed (#no_move @= (0 :: Double))
    <: #track3 @= embed (#no_move @= (0 :: Double))
    <: #_type @= 0
    <: #param @= ""
    <: emptyRecord

