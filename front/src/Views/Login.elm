module Views.Login
    ( AuthIsSuccess (AuthIsSuccess)
    , LoginAction
    , LoginFormModel
    , emptyModel
    , view
    , update
    ) where

import Signal
import List
import Maybe exposing (Maybe(..))

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events     as Ev

import Http
import Json.Decode
import Task exposing (andThen)

import Common.Tuple as Tuple

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
    { login            = ""
    , password         = ""
    , validationErrors = emptyErrors
    }

emptyErrors : ValidationErrors
emptyErrors =
    { login    = []
    , password = []
    , auth     = []
    }

image = "https://m1.behance.net/rendition/modules/34310785/disp/bf4dadf086f46af6d7331132e8ae9b02.jpg"

centeredDiv : Html -> Html
centeredDiv content =
    div [Attr.class "ui one column stackable center aligned page grid"]
        [div [Attr.class "column five wide"]
            [h1 [] <| [text (lc "Qwas")], content]]

view : LoginFormModel -> Signal.Address LoginAction -> Html
view model mailboxAddress = centeredDiv <| createLoginForm mailboxAddress model

update : LoginAction -> LoginFormModel -> UpdateTask LoginFormModel AuthIsSuccess
update action modelBefore =
    case action of
        Submit                  -> confirmLoginForm modelBefore
        UpdateLogin    login    -> Task.succeed <| UpdatedModel { modelBefore | login    <- login }
        UpdatePassword password -> Task.succeed <| UpdatedModel { modelBefore | password <- password }

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
        formHtml        =
            form [Attr.class "ui form attached segment"]
                [ Fields.validationTextField (lc "Login")    model.login    [Fields.onInput (sendAction UpdateLogin)]    isLoginValid
                , Fields.validationTextField (lc "Password") model.password [Fields.onInput (sendAction UpdatePassword)] isPasswordValid
                , Buttons.simpleButton       (lc "Sign In") [Ev.onClick mailboxAddress Submit]
                ]
        errorsPanelHtml = Fields.formMessages (List.concat [errors.login, errors.password, errors.auth]) (Just "Form errors")
    in div [] [formHtml, errorsPanelHtml]
        

confirmLoginForm : LoginFormModel -> UpdateTask LoginFormModel AuthIsSuccess
confirmLoginForm model =
    let loginErrors       = Validate.toErrorList <| Validate.notEmpty (lc "Login is empty")    model.login
        passwordErrors    = Validate.toErrorList <| Validate.notEmpty (lc "Password is empty") model.password
        noErrors          = Tuple.all List.isEmpty (passwordErrors, loginErrors)
    in
        if | noErrors  -> (checkAuthData model.login model.password) `andThen` (resolveCheckResult model)
           | otherwise ->
                let validationErrors = model.validationErrors
                in  Task.succeed <| UpdatedModel { model | password         <- ""
                                                          , validationErrors <- { validationErrors | login    <- loginErrors
                                                                                                   , password <- passwordErrors } }

checkAuthData : String -> String -> Task.Task Http.Error Bool
checkAuthData login password =
    let authUrl = "http://localhost:3000/auth/" ++ login ++ "/" ++ password
    in Http.get (Response.resultDecoder <| Json.Decode.int) authUrl
        `andThen` (\result -> Task.succeed result.success)


resolveCheckResult : LoginFormModel -> Bool -> UpdateTask LoginFormModel AuthIsSuccess
resolveCheckResult model isAuthenticated =
    let updateResult =
        if | isAuthenticated -> Action AuthIsSuccess
           | otherwise       ->
                let validationErrors = { emptyErrors | auth <- [lc "Login or password is incorrect"] }
                in  UpdatedModel { model | password         <- ""
                                         , validationErrors <- validationErrors }
    in Task.succeed updateResult