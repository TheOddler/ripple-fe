module NearbyRipples exposing (..)

import Coordinates exposing (Coordinates)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, height, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Ripple exposing (Ripple)
import Server
import Url


type alias Model =
    { nearbyRipples : List Ripple
    , seenRipples : List Ripple
    }


initModel : Model
initModel =
    { nearbyRipples = []
    , seenRipples = []
    }


initCmd : Coordinates -> Cmd Msg
initCmd startLocation =
    getList GotRipples startLocation


type Msg
    = Refresh
    | GotRipples (Result Http.Error (List Ripple))
    | ReRipple Ripple
    | GotReRippleReply (Result Http.Error ())
    | UnRipple Ripple


seconds : Float
seconds =
    1000


update : Coordinates -> Msg -> Model -> ( Model, Cmd Msg )
update coords msg model =
    case msg of
        Refresh ->
            ( model, getList GotRipples coords )

        GotRipples errOrRipples ->
            case errOrRipples of
                Err _ ->
                    ( model, Cmd.none )

                Ok ripples ->
                    ( { model | nearbyRipples = ripples }
                    , Cmd.none
                    )

        ReRipple ripple ->
            ( markRippleAsSeen ripple model
            , Cmd.none
            )

        GotReRippleReply _ ->
            ( model
            , Cmd.none
            )

        UnRipple ripple ->
            ( markRippleAsSeen ripple model
            , Cmd.none
            )


markRippleAsSeen : Ripple -> Model -> Model
markRippleAsSeen ripple model =
    let
        seenRipples =
            ripple :: model.seenRipples
    in
    { model | seenRipples = seenRipples }


unseenRipples : Model -> List Ripple
unseenRipples model =
    List.foldl List.remove model.nearbyRipples model.seenRipples


view : Model -> Html Msg
view model =
    div [ class "ripples" ]
        [ div [] <| List.map viewSingle <| unseenRipples model
        , button
            [ onClick Refresh
            , class "refresh"
            ]
            [ text "🔄 Refresh"
            ]
        ]


viewSingle : Ripple -> Html Msg
viewSingle ripple =
    div
        [ class "ripple" ]
        [ img
            [ src <| Url.toString <| Server.imgUrl ripple.id
            ]
            []
        , div [ class "re-ripple", onClick <| ReRipple ripple ] [ text "📤 Re-Ripple" ]
        , div [ class "un-ripple", onClick <| UnRipple ripple ] [ text "❌ Un-Ripple" ]
        ]


getList : (Result Http.Error (List Ripple) -> msg) -> Coordinates -> Cmd msg
getList msg coords =
    Http.get
        { url = Server.list coords |> Url.toString
        , expect = Http.expectJson msg (Decode.list Ripple.decoder)
        }


reRipple : (Result Http.Error () -> msg) -> Coordinates -> Ripple -> Cmd msg
reRipple msg currentCoords ripple =
    Http.post
        { url = Server.reRipple |> Url.toString
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "coordinates", Coordinates.toJSON currentCoords )
                    , ( "id", Encode.string ripple.id )
                    ]
        , expect = Http.expectWhatever msg
        }
