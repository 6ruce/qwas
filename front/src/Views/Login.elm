module Views.Login
    ( RedirectAfterLogin (..)
    , LoginAction (..) --remove (..) after debug
    , LoginFormModel
    , emptyModel
    , view
    , update
    ) where

import Signal
import List

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events     as Ev

import Http

import Common.Tuple as Tuple
import Localization exposing (..)
import Router       exposing (..)
import Response

import Widgets.Buttons as Buttons
import Widgets.Fields  as Fields

type RedirectAfterLogin = RedirectAfterLogin

type LoginAction
    = Submit
    | UpdateLogin String
    | UpdatePassword String

type alias LoginFormModel =
    { login    : String
    , password : String
    , validationErrors : ValidationErrors
    }

type alias ValidationErrors =
    { login    : List String
    , password : List String
    }

emptyModel =
    { login    = ""
    , password = ""
    , validationErrors = {login = [], password = []}
    }

image = "https://m1.behance.net/rendition/modules/34310785/disp/bf4dadf086f46af6d7331132e8ae9b02.jpg"

centeredDiv : Html -> Html
centeredDiv content =
    div [Attr.class "ui one column stackable center aligned page grid"]
        [div [Attr.class "column five wide"]
            [h1 [] <| [text (lc "Qwas")], content]]

view : LoginFormModel -> Signal.Address LoginAction -> Html
view model mailboxAddress = centeredDiv <| loginForm mailboxAddress model

update : LoginAction -> LoginFormModel -> UpdateResult LoginFormModel RedirectAfterLogin
update action modelBefore =
    case action of
        Submit                  -> confirmForm modelBefore
        UpdateLogin    login    -> Model { modelBefore | login    <- login }
        UpdatePassword password -> Model { modelBefore | password <- password }

-- Login Form
sendLoginAction : Signal.Address LoginAction -> (a -> LoginAction) -> a -> Signal.Message
sendLoginAction address action value = Signal.message address (action value)

loginForm : Signal.Address LoginAction -> LoginFormModel -> Html
loginForm mailboxAddress model =
    let
        errors          = model.validationErrors
        isLoginValid    = List.isEmpty errors.login
        isPasswordValid = List.isEmpty errors.password
        sendAction      = sendLoginAction mailboxAddress
    in
        form [Attr.class "ui form segment"]
            [ Fields.validationTextField (lc "Login")    model.login    [Fields.onInput (sendAction UpdateLogin)]    isLoginValid
            , Fields.validationTextField (lc "Password") model.password [Fields.onInput (sendAction UpdatePassword)] isPasswordValid
            , Fields.errorMessages       (List.concat [errors.login, errors.password])
            , Buttons.simpleButton       (lc "Sign In") [Ev.onClick mailboxAddress Submit]
            ]

confirmForm : LoginFormModel -> UpdateResult LoginFormModel RedirectAfterLogin
confirmForm model =
    let loginErrors    = checkLogin    model.login
        passwordErrors = checkPassword model.password
        noErrors       = (List.isEmpty loginErrors) && (List.isEmpty passwordErrors)
    in
        if | noErrors  -> Action RedirectAfterLogin
           | otherwise ->
                Model { model |
                    password         <- "",
                    validationErrors <- { login = loginErrors, password = passwordErrors }
                }

checkLogin : String -> List String
checkLogin login =
    if (login == "") then [lc "Login is empty"] else []

checkPassword : String -> List String
checkPassword password =
    if (password == "") then [lc "Password is empty"] else []