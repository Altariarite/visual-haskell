{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Json where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    decode,
    eitherDecode,
    object,
    withObject,
  )
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as I
import Data.Tree
import GHC.Generics (Generic)

t :: Tree Integer
t = unfoldTree buildNode 1
  where
    buildNode x = if 2 * x + 1 > 7 then (x, []) else (x, [2 * x, 2 * x + 1])


myTree :: Tree Integer
myTree = (Node {rootLabel = 7, subForest = [Node {rootLabel = 2, subForest = [Node {rootLabel = 4, subForest = []},Node {rootLabel = 10, subForest = []}]},Node {rootLabel = 3, subForest = [Node {rootLabel = 6, subForest = []},Node {rootLabel = 7, subForest = []}]}]})

{- 
>>> take 1 m
-}
m =
  [ [1, 5871, 8916, 2868],
    [1951, 10048, 2060, 6171],
    [8010, 16145, 8090, 8045],
    [1013, 990, 940, 6907]
  ]

writeJson :: ToJSON a => a -> IO ()
writeJson m = do
  let jsonString = encodeToLazyText m
  I.writeFile "out.json" jsonString

readJson = do
  input <- B.readFile "out.json"
  let mm = eitherDecode input :: Either String (Tree Integer)
  return mm



