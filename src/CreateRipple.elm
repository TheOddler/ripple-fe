module CreateRipple exposing (..)

import Coordinates exposing (Coordinates)
import File exposing (File)
import Html exposing (Html, div, img, input, label, text)
import Html.Attributes exposing (accept, attribute, class, height, hidden, name, src, type_)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as D
import Server
import Task
import Url


type alias ImagePreviewUrl =
    String


type Model
    = Nothing
    | LoadingPreview File
    | Ready File ImagePreviewUrl


initModel : Model
initModel =
    Nothing


type Msg
    = SetImage File
    | LoadedImagePreview File ImagePreviewUrl
    | Upload File
    | UploadDone (Result Http.Error ())


update : Coordinates -> Msg -> Model -> ( Model, Cmd Msg )
update location msg model =
    case msg of
        SetImage image ->
            ( LoadingPreview image
            , Task.perform (LoadedImagePreview image) (File.toUrl image)
            )

        LoadedImagePreview image preview ->
            ( Ready image preview
            , Cmd.none
            )

        Upload image ->
            ( Nothing
            , Http.post
                { url = Url.toString Server.upload
                , body =
                    Http.multipartBody
                        [ Http.stringPart "latitude" <| String.fromFloat location.latitude
                        , Http.stringPart "longitude" <| String.fromFloat location.longitude
                        , Http.filePart "image" image
                        ]
                , expect = Http.expectWhatever UploadDone
                }
            )

        UploadDone _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "createRipple" ]
        [ div [ class "preview" ]
            [ case model of
                Nothing ->
                    div [ class "info" ] [ text "Tap the camera icon to create a Ripple" ]

                LoadingPreview _ ->
                    div [ class "info" ] [ text "Loading preview..." ]

                Ready _ preview ->
                    img [ src preview, height 300 ] []
            ]
        , div [ class "overlay" ]
            [ viewMakeRippleButton Capture
            , case model of
                Nothing ->
                    text ""

                LoadingPreview file ->
                    viewRippleUploadButton file

                Ready file _ ->
                    viewRippleUploadButton file
            ]
        ]


type RippleMethod
    = Select
    | Capture


viewMakeRippleButton : RippleMethod -> Html Msg
viewMakeRippleButton rippleMethod =
    label [ class "button" ] <|
        let
            baseAttrs =
                [ type_ "file"
                , accept "image/*"
                , on "change" <| D.map SetImage inputFileDecoder
                , name "image"
                , hidden True
                ]
        in
        [ input
            (case rippleMethod of
                Select ->
                    -- Opening a selection dialog is the default, so add nothing
                    baseAttrs

                Capture ->
                    -- To capture directly from the camera add the "capture" attribute
                    attribute "capture" "environment" :: baseAttrs
            )
            []
        , div
            []
            [ text <|
                case rippleMethod of
                    Select ->
                        "📂"

                    Capture ->
                        "📸"
            ]
        ]


viewRippleUploadButton : File -> Html Msg
viewRippleUploadButton file =
    div
        [ class "button"
        , onClick <| Upload file
        ]
        [ text "📤"
        ]


inputFileDecoder : D.Decoder File
inputFileDecoder =
    D.at [ "target", "files" ] (D.oneOrMore (\first _ -> first) File.decoder)
