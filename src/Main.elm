module Main exposing (..)

import Browser
import Coordinates exposing (Coordinates)
import CreateRipple
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import NearbyRipples
import Ports exposing (watchPosition)


type Tab
    = NearbyRipples
    | CreateRipple


type alias Model =
    { location : Coordinates
    , openTab : Tab
    , createRipple : CreateRipple.Model
    , nearbyRipples : NearbyRipples.Model
    }


type Msg
    = GotLocation Coordinates
    | ChangeTab Tab
    | CreateRippleMsg CreateRipple.Msg
    | NearbyRipplesMsg NearbyRipples.Msg


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


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { location = flags.startLocation
      , openTab = NearbyRipples
      , createRipple = CreateRipple.initModel
      , nearbyRipples = NearbyRipples.initModel
      }
    , Cmd.map NearbyRipplesMsg <| NearbyRipples.initCmd flags.startLocation
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    watchPosition GotLocation


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotLocation location ->
            ( { model | location = location }
            , Cmd.none
            )

        ChangeTab tab ->
            ( { model | openTab = tab }
            , Cmd.none
            )

        CreateRippleMsg msg ->
            let
                ( newModel, cmd ) =
                    CreateRipple.update model.location msg model.createRipple
            in
            ( { model | createRipple = newModel }
            , Cmd.map CreateRippleMsg cmd
            )

        NearbyRipplesMsg msg ->
            let
                ( newModel, cmd ) =
                    NearbyRipples.update model.location msg model.nearbyRipples
            in
            ( { model | nearbyRipples = newModel }
            , Cmd.map NearbyRipplesMsg cmd
            )


view : Model -> Html Msg
view model =
    div
        []
        [ div [ class "tabs" ]
            [ viewTab model NearbyRipples
            , viewTab model CreateRipple
            ]
        , case model.openTab of
            NearbyRipples ->
                Html.map NearbyRipplesMsg <| NearbyRipples.view model.nearbyRipples

            CreateRipple ->
                Html.map CreateRippleMsg <| CreateRipple.view model.createRipple
        ]


tabLabel : Tab -> String
tabLabel tab =
    case tab of
        NearbyRipples ->
            "🗺️ Nearby"

        CreateRipple ->
            "🎨 Create"


viewTab : Model -> Tab -> Html Msg
viewTab model tab =
    div
        [ classList
            [ ( "tab", True )
            , ( "active", model.openTab == tab )
            ]
        , onClick <| ChangeTab tab
        ]
        [ text <| tabLabel tab
        ]
