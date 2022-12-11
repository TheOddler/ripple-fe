port module Geolocation exposing (..)


type alias Latitude =
    Float


type alias Longitude =
    Float


type alias Coordinates =
    { latitude : Latitude
    , longitude : Longitude
    }


{-| watchPosition matches the javascript name of the function
-}
port watchPosition : (Coordinates -> msg) -> Sub msg
