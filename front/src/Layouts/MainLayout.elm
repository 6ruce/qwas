module Layouts.MainLayout
    ( view
    ) where

import List exposing ((::))

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events     as Ev

import Http exposing (Error(..))
import Application exposing (ApplicationError)

view : List ApplicationError -> Html -> Html
view errors content =
    div [Attr.id "container", Attr.style [("width","100%"), ("height","100%")]]
        [ errorMessagePanel errors
        , content
        , node "link" [Attr.rel "stylesheet", Attr.type' "text/css", Attr.href "main.css"] []
        , node "link" [Attr.rel "stylesheet", Attr.type' "text/css", Attr.href "semantic.min.css"] []
        ]

errorMessagePanel : List ApplicationError -> Html
errorMessagePanel errors =
    div [Attr.class "ui four column centered grid"]
        [div [Attr.class "column"] 
            [div [Attr.classList [("ui bottom attached error message", True), ("hidden", List.isEmpty errors)]]
                [ div [Attr.class "header"] [text "Application Error"]
                , ul  [Attr.class "list"]   (List.map (\error -> li [] [text error]) <| formatAppErrors errors)
                ]]]
    

formatAppErrors : List ApplicationError -> List String
formatAppErrors appErrors =
    List.map (\ error ->
        case error of
            Application.NetworkError error -> "Http error: " ++ getNetworkErrorText error
            _                  -> "Something gone wrong!")
        appErrors

getNetworkErrorText : Http.Error -> String
getNetworkErrorText error =
    case error of
        Timeout                   -> "Request is timeout"
        NetworkError              -> "Problems with network"
        UnexpectedPayload message -> "Unexpected payload [" ++ message ++ "]"
        BadResponse code message  -> "Bad response " ++ (toString code) ++ "[" ++ message ++ "]"
