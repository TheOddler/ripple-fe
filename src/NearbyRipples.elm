module NearbyRipples exposing (..)

import Coordinates exposing (Coordinates)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (height, src)
import Http
import Json.Decode as Decode
import Ripple exposing (Ripple)
import Server
import Url


type alias Model =
    { nearbyRipples : List Ripple
    }


initModel : Model
initModel =
    { nearbyRipples = [] }


initCmd : Coordinates -> Cmd Msg
initCmd startLocation =
    getList GotRipples startLocation


type Msg
    = GotRipples (Result Http.Error (List Ripple))


update : Coordinates -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        GotRipples errOrRipples ->
            case errOrRipples of
                Err _ ->
                    ( model, Cmd.none )

                Ok ripples ->
                    ( { model | nearbyRipples = ripples }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div []
        [ text "Ripples:"
        , div [] <|
            List.map viewSingle model.nearbyRipples
        ]


viewSingle : Ripple -> Html msg
viewSingle ripple =
    div
        []
        [ text <| "Location: " ++ Coordinates.toString ripple.coordinates
        , img
            [ height 200
            , src <| Url.toString <| Server.imgUrl ripple.id
            ]
            []
        ]


getList : (Result Http.Error (List Ripple) -> msg) -> Coordinates -> Cmd msg
getList msg coords =
    Http.get
        { url = Server.list coords |> Url.toString
        , expect = Http.expectJson msg (Decode.list Ripple.decoder)
        }
