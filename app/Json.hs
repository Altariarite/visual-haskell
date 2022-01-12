{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    decode,
    object,
    withObject,
    eitherDecode
  )
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as I
import Data.Tree
import GHC.Generics (Generic)
import Web.Scotty

t = unfoldTree buildNode 1
  where
    buildNode x = if 2 * x + 1 > 7 then (x, []) else (x, [2 * x, 2 * x + 1])

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
  print mm

data User = User {userId :: Int, userName :: String} deriving (Show, Generic)

instance ToJSON User

instance FromJSON User

bob :: User
bob = User {userId = 1, userName = "bob"}

jenny :: User
jenny = User {userId = 2, userName = "jenny"}

allUsers :: [User]
allUsers = [bob, jenny]

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

startServer = do
  putStrLn "Starting server..."
  scotty 3000 $ do
    get "/hello/:name" $ do
      name <- param "name"
      text ("hello " <> name <> "!")

    get "/users" $ do
      json allUsers

    get "/users/:id" $ do
      id <- param "id"
      json (filter (matchesId id) allUsers)
    post "/users" $ do
      user <- jsonData :: ActionM User
      json user
