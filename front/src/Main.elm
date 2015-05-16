module Qwas where

import Html exposing (..)
import Signal
import Task exposing ( Task
                     , andThen
                     , onError
                     , mapError )

import Http
import Maybe exposing (Maybe(..))

import Application  exposing (ApplicationError(..))
import Localization exposing (..)
import Router       exposing (..)

import Layouts.MainLayout as MainLayout
import Views.Login        as LoginM exposing (LoginAction, LoginFormModel)
import Views.MainPage     as MainPageM

type Action
    = None
    | SignIn LoginAction
    | UpdateModel Model

type Page
    = Login
    | MainPage

-- Application state
type Model =
    Model {
    -- General application state
      pageTitle     : String
    , currentPage   : Page
    , updateTask    : Maybe (Task () Model)
    , authenticated : Bool
    , errors        : List ApplicationError

    -- Login page state
    , loginForm : LoginFormModel

    }

emptyModel : Model
emptyModel =
    Model
        { pageTitle     = lc "Qwas"
        , currentPage   = Login
        , updateTask    = Nothing
        , authenticated = False
        , errors        = []

        -- Modules empty models
        , loginForm     = LoginM.emptyModel
        }

-- View Model Update
main : Signal Html
main = Signal.map view modelMailbox.signal

view : Model -> Html
view (Model model) =
    let content =
        case model.currentPage of
            Login    -> LoginM.view model.loginForm loginAddress
            MainPage -> MainPageM.view
    in MainLayout.view model.errors content

model : Signal Model
model =
    Signal.foldp update emptyModel mainMailbox.signal

update : Action -> Model -> Model
update action (Model model) =
    case action of
        SignIn loginAction ->
            let loginUpdateTask = LoginM.update loginAction model.loginForm
                updateTask = loginUpdateTask
                    `andThen` (\updateResult ->
                        case updateResult of
                            Action (LoginM.AuthIsSuccess) -> Task.succeed <| Model { model | authenticated <- True, currentPage <- MainPage }
                            UpdatedModel loginModel       -> Task.succeed <| Model { model | loginForm     <- loginModel })
                    `andThen` (\(Model updatedModel) -> Task.succeed <| Model { updatedModel | errors <- [] })
                    `onError` (showHttpError <| Model model)
            in Model { model | updateTask <- Just updateTask }
        UpdateModel (Model newModel) -> Model { newModel | updateTask <- Nothing }
        _                            -> Model model

showHttpError : Model -> Http.Error -> Task () Model
showHttpError (Model model) error =                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
    Task.succeed <| Model { model | errors <- [NetworkError error] }

-- Mailboxes
modelMailbox : Signal.Mailbox Model
modelMailbox = Signal.mailbox emptyModel

mainMailbox : Signal.Mailbox Action
mainMailbox = Signal.mailbox None

loginAddress : Signal.Address LoginAction
loginAddress =
    Signal.forwardTo mainMailbox.address SignIn

-- Ports
getData : Model -> Task () (List ())
getData (Model model) =
    case model.updateTask of
        Just task -> task
            `andThen` (\model -> Task.sequence [ Signal.send modelMailbox.address model
                                               , Signal.send mainMailbox.address (UpdateModel model)])
        Nothing -> Task.succeed []

port httpTasks : Signal (Task () (List ()))
port httpTasks =
    Signal.map getData model

