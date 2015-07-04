module Widgets.Fields where

import List
import Signal
import Maybe exposing (Maybe(..))

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Ev

-- Events
onInput : (String -> Signal.Message) -> Attribute
onInput message = Ev.on "input" Ev.targetValue message

-- Inputs
textInput : String -> String -> List Attribute -> Html
textInput placeholder value additionalAttributes =
    div [Attr.class "ui input"]
        [input (List.concat [[Attr.type' "text", Attr.placeholder placeholder, Attr.value value], additionalAttributes]) []]

-- Fields
textField : String -> String -> List Attribute -> Html
textField placeholder value additionalAttributes =
    let
        input = textInput placeholder value additionalAttributes
    in
        div [Attr.class "required field"]
            [input]

validationTextField : String -> String ->List Attribute -> Bool -> Html
validationTextField placeholder value additionalAttributes isValid =
    let
        input = textInput placeholder value additionalAttributes
    in
        div [Attr.classList [("required field", True), ("error", not isValid)]]
            [input]

-- Form Messages
formMessages : List String -> Maybe String -> Html
formMessages errors maybeTitle =
    let title = case maybeTitle of
                    Just title -> [text title]
                    _          -> []
        isHidden      = List.isEmpty errors
        errorMessage message = li [] [text message]
    in div [Attr.classList [("ui bottom attached warning message left aligned column", True), ("hidden", isHidden)]]
            [ div [Attr.class "header"] title
            , ul  [Attr.class "list"] <| List.map errorMessage errors
            ]