module Widgets.Fields where

import List
import Signal

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
errorMessages : List String -> Html
errorMessages errors =
    let
        displayValue         = if List.isEmpty errors then "none" else "block"
        display              = Attr.style [("display", displayValue)]
        errorMessage message = li [] [text message]
    in div [Attr.class "ui error message left aligned column", display]
           [ul [] <| List.map errorMessage errors]