module Server exposing (imgUrl, list, upload)

import Coordinates exposing (Coordinates)
import Url exposing (Url)


baseUrl : Url
baseUrl =
    { protocol = Url.Http
    , host = "localhost"
    , port_ = Just 8000
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }


list : Coordinates -> Url
list coords =
    { baseUrl
        | path = "/list"
        , query =
            Just <|
                "latitude="
                    ++ String.fromFloat coords.latitude
                    ++ "&longitude="
                    ++ String.fromFloat coords.longitude
    }


imgUrl : String -> Url
imgUrl imgID =
    { baseUrl | path = "/ripple/" ++ imgID }


upload : Url
upload =
    { baseUrl | path = "/upload" }
