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
import Controller   exposing (Page(..))

import Layouts.MainLayout as MainLayout

type Action
    = None
    | Application Controller.Action
    | UpdateModel Model

-- Application state
type Model =
    Model {
    -- General application state
      pageTitle     : String
    , updateTask    : Maybe (Task () Model)
    , errors        : List ApplicationError

    -- Modules state
    , state : Controller.State
    }

emptyModel : Model
emptyModel =
    Model
        { pageTitle     = lc "Qwas"
        , updateTask    = Nothing
        , errors        = []
        , state         = Controller.emptyState
        }

-- View Model Update
main : Signal Html
main = Signal.map view modelMailbox.signal

view : Model -> Html
view (Model model) =
    let content = Controller.viewContent model.state applicationActionAddress
    in
        MainLayout.view model.errors content

model : Signal Model
model =
    Signal.foldp update emptyModel mainMailbox.signal

update : Action -> Model -> Model
update action (Model model) =
    case action of
        Application appAction ->
            let updateTask = Controller.handle appAction model.state
                `andThen` (setState <| Model model)
                `andThen` cleanAppErrors
                `onError` (showHttpError <| Model model)
            in Model { model | updateTask <- Just updateTask }
        UpdateModel (Model newModel) -> Model { newModel | updateTask <- Nothing }
        _                            -> Model model

setState : Model -> Controller.State -> Task Http.Error Model
setState (Model model) state =
    Task.succeed <| Model { model | state <- state }

-- Application error handling
showHttpError : Model -> Http.Error -> Task () Model
showHttpError (Model model) error =                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
    Task.succeed <| Model { model | errors <- [NetworkError error] }

cleanAppErrors : Model -> Task Http.Error Model
cleanAppErrors (Model updatedModel) =
    Task.succeed <| Model { updatedModel | errors <- [] }

-- Mailboxes
modelMailbox : Signal.Mailbox Model
modelMailbox = Signal.mailbox emptyModel

mainMailbox : Signal.Mailbox Action
mainMailbox = Signal.mailbox None

applicationActionAddress : Signal.Address Controller.Action
applicationActionAddress =
    Signal.forwardTo mainMailbox.address Application

-- Trigger model changing signal for two mailboxes
sendModel : Model -> Task () (List ())
sendModel (Model model) =
    case model.updateTask of
        Just task -> task
            `andThen` (\model -> Task.sequence [ Signal.send modelMailbox.address model
                                               , Signal.send mainMailbox.address (UpdateModel model)])
        Nothing -> Task.succeed []

port httpTasks : Signal (Task () (List ()))
port httpTasks =
    Signal.map sendModel model

