module Views.MainPage
    ( view
    ) where

import Html exposing (Html)

import Localization exposing (lc)

import Widgets.Buttons exposing (..)

view : Html
view = simpleButton (lc "Test") []