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
import Json.Decode
import Task exposing (andThen)

import Common.Tuple as Tuple
import Common.Bools as Bools

import Localization exposing (..)
import Router       exposing (..)
import Response

import Widgets.Buttons as Buttons
import Widgets.Fields  as Fields

import Validators.Text as Validate

type AuthIsSuccess = AuthIsSuccess

type LoginAction
    = Submit
    | UpdateLogin    String
    | UpdatePassword String

type alias LoginFormModel =
    { login    : String
    , password : String
    , validationErrors : ValidationErrors
    }

type alias ValidationErrors =
    { login    : List String
    , password : List String
    , auth     : List String
    }

emptyModel =
    { login    = ""
    , password = ""
    , validationErrors = { login = [], password = [], auth = [] }
    }

image = "https://m1.behance.net/rendition/modules/34310785/disp/bf4dadf086f46af6d7331132e8ae9b02.jpg"

centeredDiv : Html -> Html
centeredDiv content =
    div [Attr.class "ui one column stackable center aligned page grid"]
        [div [Attr.class "column five wide"]
            [h1 [] <| [text (lc "Qwas")], content]]

view : LoginFormModel -> Signal.Address LoginAction -> Html
view model mailboxAddress = centeredDiv <| createLoginForm mailboxAddress model

update : LoginAction -> LoginFormModel -> UpdateResult LoginFormModel AuthIsSuccess
update action modelBefore =
    case action of
        Submit                  -> confirmLoginForm modelBefore
        UpdateLogin    login    -> Model { modelBefore | login    <- login }
        UpdatePassword password -> Model { modelBefore | password <- password }

-- Login Form
sendLoginAction : Signal.Address LoginAction -> (a -> LoginAction) -> a -> Signal.Message
sendLoginAction address action value = Signal.message address (action value)

createLoginForm : Signal.Address LoginAction -> LoginFormModel -> Html
createLoginForm mailboxAddress model =
    let
        errors          = model.validationErrors
        isLoginValid    = List.isEmpty errors.login
        isPasswordValid = List.isEmpty errors.password
        sendAction      = sendLoginAction mailboxAddress
    in
        form [Attr.class "ui form segment"]
            [ Fields.validationTextField (lc "Login")    model.login    [Fields.onInput (sendAction UpdateLogin)]    isLoginValid
            , Fields.validationTextField (lc "Password") model.password [Fields.onInput (sendAction UpdatePassword)] isPasswordValid
            , Fields.errorMessages       (List.concat [errors.login, errors.password, errors.auth])
            , Buttons.simpleButton       (lc "Sign In") [Ev.onClick mailboxAddress Submit]
            ]

confirmLoginForm : LoginFormModel -> UpdateResult LoginFormModel AuthIsSuccess
confirmLoginForm model =
    let loginErrors    = checkLogin    model.login
        passwordErrors = checkPassword model.password
        noErrors       = Tuple.all List.isEmpty (passwordErrors, loginErrors)
    in
        if | noErrors  -> Action AuthIsSuccess
           | otherwise ->
                Model { model |
                    password         <- "",
                    validationErrors <- { login = loginErrors, password = passwordErrors }
                }

confirmLoginForm1 : LoginFormModel -> UpdateTask LoginFormModel AuthIsSuccess
confirmLoginForm1 model =
    let loginErrors       = Validate.toErrorList <| Validate.notEmpty (lc "Login is empty")    model.login
        passwordErrors    = Validate.toErrorList <| Validate.notEmpty (lc "Password is empty") model.password
        noErrors          = Tuple.all List.isEmpty (passwordErrors, loginErrors)
    in
        if | noErrors  -> checkAuthData model.login model.password `andThen` resolveCheckResult model
           | otherwise -> Task.succeed <|
                Model { model |
                  password         <- "",
                  validationErrors <- { login = loginErrors, password = passwordErrors }
                }

checkAuthData : String -> String -> Task.Task Http.Error Bool
checkAuthData login password =
    Http.get (Response.resultDecoder <| Json.Decode.null "") "http://localhost:3000/auth"
        `andThen` (\result -> Task.succeed result.success)

resolveCheckResult : LoginFormModel -> Bool -> UpdateTask LoginFormModel AuthIsSuccess
resolveCheckResult model isAuthenticated =
    if | isAuthenticated -> Task.succeed <| Action AuthIsSuccess
       | otherwise       -> Task.succeed <|
            Model { model |
              password         <- "",
              validationErrors <- { model.validationErrors | auth <- [lc "Authentication failed"] }
            }
