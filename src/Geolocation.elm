port module Geolocation exposing (..)

-- import Time exposing (Posix)


type alias Location =
    { latitude : Float
    , longitude : Float
    , accuracy : Float
    , altitude : Maybe Altitude
    , movement : Maybe Movement

    -- , timestamp : Posix
    }


type alias Altitude =
    { metersAboveSeaLevel : Float
    , accuracy : Float
    }


type alias Movement =
    { speed : Float

    -- The heading as degrees clockwise from North
    , heading : Float
    }


port watchPosition : (Location -> msg) -> Sub msg
