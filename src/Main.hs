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

instance ToJSON Person

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Value
getHomeR = returnJson $ Person "nAME" 12

main :: IO ()
main = warp 3000 App