module Qwas where

import Html exposing (..)
import Signal
import Task exposing ( Task
                     , andThen
                     , onError
                     , mapError )
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
    , errors        : List String

    -- Login page state
    , loginForm : LoginFormModel

    }

emptyModel : Model
emptyModel =
    { pageTitle     = lc "Qwas"
    , currentPage   = Login
    , authenticated = True
    , errors        = []

    -- Modules empty models
    , loginForm     = LoginM.emptyModel
    }

-- View Model Update
main : Signal Html
main = Signal.map view modelMailbox.signal

view : Model -> Html
view model =
    case model.currentPage of
        Login    -> LoginM.view model.loginForm loginAddress
        MainPage -> MainPageM.view

model : Signal (Task () Model)
model =
    Signal.foldp update (Task.succeed emptyModel) mainMailbox.signal

update : Action -> Task () Model -> Task () Model
update action modelTask =
    modelTask `andThen` \model ->
        case action of
            SignIn loginAction ->
                let updateResultTask = LoginM.update loginAction model.loginForm
                in  updateResultTask
                    `andThen` (\updateResult ->
                        case updateResult of
                            Action (LoginM.AuthIsSuccess) -> Task.succeed { model | authenticated <- True, currentPage <- MainPage }
                            Model  loginModel             -> Task.succeed { model | loginForm     <- loginModel })
                    `onError` (showHttpError model)
            _ -> Task.succeed model

showHttpError : Model -> Http.Error -> Task () Model
showHttpError model error =
    -- TODO: Show proper Http.Error messages 
    Task.succeed { model | errors <- [lc "Http error"] }

-- Mailboxes
modelMailbox : Signal.Mailbox Model
modelMailbox = Signal.mailbox emptyModel

mainMailbox : Signal.Mailbox Action
mainMailbox = Signal.mailbox None

loginAddress : Signal.Address LoginAction
loginAddress =
    Signal.forwardTo mainMailbox.address SignIn

-- Ports
getData : Task () Model -> Task () ()
getData modelTask =
    modelTask `andThen` \model -> Signal.send modelMailbox.address model

port httpTasks : Signal (Task () ())
port httpTasks =
    Signal.map getData model

