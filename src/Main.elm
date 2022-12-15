module Main exposing (..)

import Browser
import Coordinates exposing (Coordinates)
import File exposing (File)
import Html exposing (Html, div, img, input, label, text)
import Html.Attributes exposing (accept, attribute, hidden, src, style, type_)
import Html.Events exposing (on)
import Http
import Json.Decode as D
import Ports exposing (watchPosition)
import Ripple exposing (Ripple)
import Task


type alias ImagePreviewUrl =
    String


type alias LocalRippleImage =
    { image : File, preview : ImagePreviewUrl }


type alias Model =
    { localRipple : Maybe LocalRippleImage
    , location : Maybe Coordinates
    , remoteRipples : List Ripple
    }


type Msg
    = SetLocalRippleImage File
    | SetLocalRippleImagePreview File ImagePreviewUrl
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
    { localRipple = Nothing
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
update msg model =
    case msg of
        SetLocalRippleImage image ->
            ( model
            , Task.perform (SetLocalRippleImagePreview image) (File.toUrl image)
            )

        SetLocalRippleImagePreview image preview ->
            ( { model | localRipple = Just { image = image, preview = preview } }
            , Cmd.none
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
        , viewMakeRippleButton
            Select
            SetLocalRippleImage
            [ div [ style "font-size" "100px" ] [ text "📂" ]
            ]
        , viewMakeRippleButton
            Capture
            SetLocalRippleImage
            [ div [ style "font-size" "100px" ] [ text "📸" ]
            ]
        , case model.localRipple of
            Nothing ->
                div []
                    [ text "No ripple selected"
                    ]

            Just image ->
                img [ src image.preview ]
                    []
        , text "Ripples:"
        , div [] <|
            List.map Ripple.view model.remoteRipples
        ]


type RippleMethod
    = Select
    | Capture


viewMakeRippleButton : RippleMethod -> (File -> msg) -> List (Html msg) -> Html msg
viewMakeRippleButton rippleMethod onChange innerHtml =
    label [] <|
        let
            baseAttrs =
                [ type_ "file"
                , accept "image/*"
                , on "change" <| D.map onChange inputFileDecoder
                , hidden True
                ]
        in
        input
            (case rippleMethod of
                Select ->
                    -- Opening a selection dialog is the default, so add nothing
                    baseAttrs

                Capture ->
                    -- To capture directly from the camera add the "capture" attribute
                    attribute "capture" "environment" :: baseAttrs
            )
            []
            :: innerHtml


inputFileDecoder : D.Decoder File
inputFileDecoder =
    D.at [ "target", "files" ] (D.oneOrMore (\first _ -> first) File.decoder)
