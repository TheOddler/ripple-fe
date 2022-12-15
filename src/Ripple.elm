module Ripple exposing (..)

import Coordinates exposing (Coordinates)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (height, src)
import Http
import Json.Decode as Decode exposing (Decoder)
import Server
import Url


type alias RippleID =
    String


type alias Ripple =
    { id : RippleID
    , coordinates : Coordinates
    }


decoder : Decoder Ripple
decoder =
    Decode.map2 Ripple
        (Decode.field "id" Decode.string)
        (Decode.field "coordinates" Coordinates.decoder)


getList : (Result Http.Error (List Ripple) -> msg) -> Coordinates -> Cmd msg
getList msg coords =
    Http.get
        { url = Server.list coords |> Url.toString
        , expect = Http.expectJson msg (Decode.list decoder)
        }


view : Ripple -> Html msg
view ripple =
    div
        []
        [ text <|
            "Location: "
                ++ String.fromFloat ripple.coordinates.longitude
                ++ " - "
                ++ String.fromFloat ripple.coordinates.latitude
        , img
            [ height 200
            , src <| Url.toString <| Server.imgUrl ripple.id
            ]
            []
        ]
