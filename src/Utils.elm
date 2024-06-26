module Utils exposing (..)

import Http

errorToString : Http.Error -> String
errorToString error =
  case error of
    Http.BadUrl url ->
        "The URL " ++ url ++ " was invalid"
    Http.Timeout ->
        "Unable to reach the server"
    Http.NetworkError ->
        "Network Error"
    Http.BadStatus 500 ->
        "500: Internal Server Error"
    Http.BadStatus 400 ->
        "400: Bad Request"
    Http.BadStatus code ->
        (String.fromInt code) ++ ": Unknown error"
    Http.BadBody errorMessage ->
        errorMessage
