module CreateRipple exposing (..)

import Coordinates exposing (Coordinates)
import File exposing (File)
import Html exposing (Html, button, div, img, input, label, text)
import Html.Attributes exposing (accept, attribute, height, hidden, name, src, style, type_)
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
            ( model
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
    div []
        [ viewMakeRippleButton Capture
        , case model of
            Nothing ->
                text "Select an image be pressing the button above"

            LoadingPreview _ ->
                text "Loading preview..."

            Ready _ preview ->
                img [ src preview, height 300 ] []
        , case model of
            Nothing ->
                text ""

            LoadingPreview _ ->
                text ""

            Ready file _ ->
                viewRippleUploadButton file
        ]


type RippleMethod
    = Select
    | Capture


viewMakeRippleButton : RippleMethod -> Html Msg
viewMakeRippleButton rippleMethod =
    label [] <|
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
            [ style "font-size" "100pt"
            ]
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
    button
        [ onClick <| Upload file ]
        [ text "Upload"
        ]


inputFileDecoder : D.Decoder File
inputFileDecoder =
    D.at [ "target", "files" ] (D.oneOrMore (\first _ -> first) File.decoder)
