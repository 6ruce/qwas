module Router where

import Task
import Http

type UpdateResult m a 
    =  Model m 
    | Action a

type alias UpdateTask m a = Task.Task Http.Error (UpdateResult m a)