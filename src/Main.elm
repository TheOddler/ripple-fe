module Main exposing (..)

import Browser
import File exposing (File)
import Html exposing (Html, div, img, input, label, text)
import Html.Attributes exposing (accept, attribute, hidden, src, style, type_)
import Html.Events exposing (on)
import Json.Decode as D
import Task


type alias ImagePreviewUrl =
    String


type alias LocalRippleImage =
    { image : File, preview : ImagePreviewUrl }


type alias Model =
    { localRipple : Maybe LocalRippleImage
    }


type Msg
    = SetLocalRippleImage File
    | SetLocalRippleImagePreview File ImagePreviewUrl


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { localRipple = Nothing }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


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


view : Model -> Html Msg
view model =
    div
        []
        [ text "Hello test"
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
