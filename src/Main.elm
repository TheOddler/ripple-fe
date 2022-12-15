module Main exposing (..)

import Browser
import Coordinates exposing (Coordinates)
import CreateRipple
import Html exposing (Html, div, text)
import Http
import Ports exposing (watchPosition)
import Ripple exposing (Ripple)


type alias Model =
    { createRipple : CreateRipple.Model
    , location : Maybe Coordinates
    , remoteRipples : List Ripple
    }


type Msg
    = CreateRippleMsg CreateRipple.Msg
    | GotLocation Coordinates
    | GotRipples (Result Http.Error (List Ripple))


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


initModel : Model
initModel =
    { createRipple = CreateRipple.initModel
    , location = Nothing
    , remoteRipples = []
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Ripple.getList GotRipples { longitude = 0, latitude = 0 }
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    watchPosition GotLocation


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        CreateRippleMsg msg ->
            let
                ( newModel, cmd ) =
                    CreateRipple.update msg model.createRipple
            in
            ( { model | createRipple = newModel }
            , Cmd.map CreateRippleMsg cmd
            )

        GotLocation location ->
            ( { model | location = Just location }
            , Cmd.none
            )

        GotRipples errOrRipples ->
            case errOrRipples of
                Err _ ->
                    ( model, Cmd.none )

                Ok ripples ->
                    ( { model | remoteRipples = ripples }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div
        []
        [ case model.location of
            Nothing ->
                text "No location found"

            Just location ->
                text <|
                    "Location: "
                        ++ String.fromFloat location.longitude
                        ++ " - "
                        ++ String.fromFloat location.latitude
        , Html.map CreateRippleMsg <| CreateRipple.view model.createRipple
        , text "Ripples:"
        , div [] <|
            List.map Ripple.view model.remoteRipples
        ]
