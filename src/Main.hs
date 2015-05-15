{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DeriveGeneric      #-}

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
/auth AuthR GET
|]

instance Yesod App

getHomeR :: Handler Value
getHomeR = returnJson $ Person "nAME" 12

getAuthR :: Handler Value
getAuthR = returnJson $ (Result  False 0 [] :: Main.Result Int)

main :: IO ()
main = warp 3000 App