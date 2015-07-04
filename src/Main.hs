{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ViewPatterns       #-}

module Main where

import           Data.Text (Text)
import           GHC.Generics
import           Yesod
import           Text.JSON

data Person = Person
    { name :: Text
    , age  :: Int
    } deriving (Generic)

data Result a = Result
    { success  :: Bool
    , response :: a
    , errors   :: [Text]
    } deriving (Generic)

instance ToJSON Person
instance ToJSON (Main.Result Int)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/auth/#Text/#Text AuthR GET
|]

instance Yesod App

getHomeR :: Handler Value
getHomeR = returnJson $ Person "nAME" 12

getAuthR :: Text -> Text -> Handler Value
getAuthR login password =
    let isCorrectCredentials = login == "Admin" && password == "Admin"
    in returnJson $ (Result isCorrectCredentials 0 [] :: Main.Result Int)


main :: IO ()
main = warp 3000 App