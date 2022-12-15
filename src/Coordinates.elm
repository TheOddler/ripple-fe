module Coordinates exposing (Coordinates, decoder, toJSON, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Latitude =
    Float


type alias Longitude =
    Float


type alias Coordinates =
    { latitude : Latitude
    , longitude : Longitude
    }


decoder : Decoder Coordinates
decoder =
    Decode.map2 Coordinates
        (Decode.field "latitude" Decode.float)
        (Decode.field "longitude" Decode.float)


toJSON : Coordinates -> Encode.Value
toJSON coords =
    Encode.object
        [ ( "latitude", Encode.float coords.latitude )
        , ( "longitude", Encode.float coords.longitude )
        ]


toString : Coordinates -> String
toString =
    Encode.encode 0 << toJSON
