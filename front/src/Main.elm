module Qwas where

import Html exposing (..)
import Signal
import Task
import Http

import Localization exposing (..)
import Router       exposing (..)

import Views.Login    as LoginM exposing (LoginAction, LoginFormModel)
import Views.MainPage as MainPageM

type Action
    = None
    | SignIn LoginAction

type Page
    = Login
    | MainPage

-- Application state
type alias Model =
    {
    -- General application state
      pageTitle     : String
    , currentPage   : Page
    , authenticated : Bool

    -- Login page state
    , loginForm : LoginFormModel

    }

emptyModel : Model
emptyModel =
    { pageTitle     = lc "Qwas"
    , currentPage   = Login
    , authenticated = True

    -- Modules empty models
    , loginForm     = LoginM.emptyModel
    }

-- View Model Update
main : Signal Html
main = Signal.map view model

view : Model -> Html
view model =
    case model.currentPage of
        Login    -> LoginM.view model.loginForm loginAddress
        MainPage -> MainPageM.view

model : Signal Model
model =
    Signal.foldp update emptyModel mainMailbox.signal

update : Action -> Model -> Model
update action model =
    case action of
        SignIn loginAction ->
            let updateResult = LoginM.update loginAction model.loginForm
            in case updateResult of
                Action (LoginM.AuthIsSuccess) -> { model | authenticated <- True, currentPage <- MainPage }
                Model  loginModel             -> { model | loginForm     <- loginModel }
        _ -> model

-- Mailboxes
mainMailbox : Signal.Mailbox Action
mainMailbox = Signal.mailbox None

portsMailbox : Signal.Mailbox Action
portsMailbox = Signal.mailbox None

loginAddress : Signal.Address LoginAction
loginAddress =
    Signal.forwardTo mainMailbox.address SignIn

-- Ports
getData : Action -> Task.Task Http.Error String
getData action =
    case action of
        None               -> Task.succeed ""
        SignIn loginAction ->
            case loginAction of
                LoginM.Submit -> Http.getString "http://localhost:3000/auth"
                _             -> Task.succeed ""

port httpGetData : Signal (Task.Task Http.Error String)
port httpGetData =
    Signal.map getData mainMailbox.signal

