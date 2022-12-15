module Main exposing (..)

import Browser
import Coordinates exposing (Coordinates)
import CreateRipple
import Html exposing (Html, div, text)
import Http
import Ports exposing (watchPosition)
import Ripple exposing (Ripple)


type alias Model =
    { location : Coordinates
    , createRipple : CreateRipple.Model
    , remoteRipples : List Ripple
    }


type Msg
    = CreateRippleMsg CreateRipple.Msg
    | GotLocation Coordinates
    | GotRipples (Result Http.Error (List Ripple))


type alias Flags =
    { startLocation : Coordinates
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


initModel : Flags -> Model
initModel flags =
    { createRipple = CreateRipple.initModel
    , location = flags.startLocation
    , remoteRipples = []
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel flags
    , Ripple.getList GotRipples flags.startLocation
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
                    CreateRipple.update model.location msg model.createRipple
            in
            ( { model | createRipple = newModel }
            , Cmd.map CreateRippleMsg cmd
            )

        GotLocation location ->
            ( { model | location = location }
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
        [ text <| "Location: " ++ Coordinates.toString model.location
        , Html.map CreateRippleMsg <| CreateRipple.view model.createRipple
        , text "Ripples:"
        , div [] <|
            List.map Ripple.view model.remoteRipples
        ]
