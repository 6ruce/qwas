module Router where

import Task
import Http

type UpdateResult m a
    = UpdatedModel m
    | Action a

type alias UpdateTask m a = Task.Task Http.Error (UpdateResult m a)