module Controller
    ( Page (..)
    , State
    , emptyState
    , viewContent

    , handleLogin
    ) where

import Task exposing (Task, andThen)
import Http

import Router exposing (UpdateResult(..))
import Views.Login    as LoginM exposing (LoginAction, LoginFormModel)
import Views.MainPage as MainPageM

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

viewContent state loginAddress =
    case state.currentPage of
        Login    -> LoginM.view state.loginForm loginAddress
        MainPage -> MainPageM.view

handleLogin : LoginAction -> State -> Task Http.Error State
handleLogin loginAction state =
    let loginUpdateTask = LoginM.update loginAction state.loginForm
    in  loginUpdateTask
            `andThen` (\updateResult ->
                case updateResult of
                    Action (LoginM.AuthIsSuccess) -> Task.succeed <| { state | authenticated <- True, currentPage <- MainPage }
                    UpdatedModel loginModel       -> Task.succeed <| { state | loginForm     <- loginModel })