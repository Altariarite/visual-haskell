{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json where

import Data.Aeson
  ( FromJSON,
    KeyValue ((.=)),
    ToJSON (toJSON),
    decode,
    object,
  )
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as I
import GHC.Generics (Generic)
import Web.Scotty

data Person = Person
  { firstName :: !Text,
    lastName :: !Text,
    age :: Int,
    likesPizza :: Bool
  }
  deriving (Show, Generic)

p1 = Person "John" "Doe" 18 True :: Person

instance ToJSON Person

instance FromJSON Person

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Generic)

instance ToJSON a => ToJSON (Tree a) where
  toJSON Nil = object []
  toJSON (Node val l r) =
    object
      [ "name" .= val,
        "children" .= [toJSON l, toJSON r]
      ]

t = Node 1 (Node 2 Nil Nil) (Node 3 Nil Nil) :: Tree Int

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
  let mm = decode input :: Maybe [[Integer]]
  case mm of
    Nothing -> print "error parsing Json"
    Just m -> print m

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