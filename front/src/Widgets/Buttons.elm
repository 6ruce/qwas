module Widgets.Buttons where

import List

import Html exposing (..)
import Html.Attributes as Attr

simpleButton : String -> List Attribute-> Html
simpleButton caption additionalAttributes =
    div (List.concat [[Attr.class "ui button purple"], additionalAttributes])
        [text caption]