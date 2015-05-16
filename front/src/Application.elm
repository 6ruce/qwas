module Application
    ( ApplicationError (..)
    ) where

import Http

type ApplicationError
    = NetworkError Http.Error
    | OtherError String