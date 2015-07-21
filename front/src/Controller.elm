module Controller
    ( Action (..)
    , Page (..)
    , State
    , emptyState
    , viewContent
    , handle
    ) where

import Signal exposing (Address)
import Task   exposing (Task, andThen)
import Http

import Router exposing (UpdateResult(..))
import Views.Login    as LoginM exposing (LoginAction, LoginFormModel)
import Views.MainPage as MainPageM

type Action = SignIn LoginAction

type Page
    = Login
    | MainPage

type alias State =
    { currentPage   : Page
    , authenticated : Bool

    -- Login page state
    , loginForm : LoginFormModel
    }

emptyState : State
emptyState = 
    { currentPage   = Login
    , authenticated = False
    , loginForm     = LoginM.emptyModel
    }

viewContent state actionAddress =
    case state.currentPage of
        Login    -> LoginM.view state.loginForm (direcActionToAddress SignIn actionAddress)
        MainPage -> MainPageM.view

handle : Action -> State -> Task Http.Error State
handle action state =
    case action of
      SignIn loginAction -> handleLogin loginAction state

handleLogin : LoginAction -> State -> Task Http.Error State
handleLogin loginAction state =
    let loginUpdateTask = LoginM.update loginAction state.loginForm
    in  loginUpdateTask
            `andThen` (\updateResult ->
                case updateResult of
                    Action (LoginM.AuthIsSuccess) -> Task.succeed <| { state | authenticated <- True, currentPage <- MainPage }
                    UpdatedModel loginModel       -> Task.succeed <| { state | loginForm     <- loginModel })

direcActionToAddress : (a -> Action) -> Address Action -> Address a
direcActionToAddress action actionAddress = Signal.forwardTo actionAddress action