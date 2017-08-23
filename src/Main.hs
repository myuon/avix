{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Bookkeeper
import Bookkeeper.Lens
import Bookkeeper.JSON
import Data.Aeson
import Control.Lens
import Linear.V2
import GHC.OverloadedLabels

type Exedit = Book
  [ "width" :=> Int
  , "height" :=> Int
  , "rate" :=> Int
  , "scale" :=> Double
  , "length" :=> Int
  , "audio_rate" :=> Int
  , "audio_ch" :=> Int
  ]

defExedit :: Exedit
defExedit = emptyBook
  & #width =: 1280
  & #height =: 720
  & #rate =: 30
  & #scale =: 1
  & #length =: 1
  & #audio_rate =: 44100
  & #audio_ch =: 2

_size :: Lens' Exedit (V2 Int)
_size = lens (\r -> V2 (r ^. #width) (r ^. #height)) (\r v -> r & #width .~ v ^. _x & #height .~ v ^. _y)

data Resolution = VGA | HD | FullHD | Niconico | Custom (V2 Int)

_resolution :: Getter Resolution (V2 Int)
_resolution = to $ \case
  VGA -> V2 640 480
  HD -> V2 1280 720
  FullHD -> V2 1920 1080
  Niconico -> V2 960 540
  Custom v -> v

printExo :: Exedit -> IO ()
printExo ex = do
  putStrLn "[exedit]"
  mapM_ (\(k,v) -> putStrLn $ k ++ "=" ++ v) $ bookPairs ex

main :: IO ()
main = do
  printExo defExedit

