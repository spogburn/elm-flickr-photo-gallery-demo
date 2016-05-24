module Main exposing (..)

import Html exposing (Html, div, img, li, ol, text)
import Html.App as Html
import Html.Attributes exposing (src)
import Html.Events exposing (..)
import Http
import Json.Decode as Decoder
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Task


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


infixl 2 =>
(=>) =
    (,)



-- user id 37993285@N06
-- user nsid 37993285@N06


type alias Photo =
    { id : String
    , title : String
    , thumbnailUrl : String
    }


type alias Model =
    { photos : List Photo
    , selectedId : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { photos = []
            , selectedId = Nothing
            }
    in
        ( model, flickrGetPhotos )


type Msg
    = FetchSucceed (List Photo)
    | FetchFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchSucceed photos' ->
            ( { model | photos = photos' }, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )


viewThumbnail : Photo -> Html Msg
viewThumbnail { thumbnailUrl } =
    li []
        [ img [ src thumbnailUrl ] []
        ]


view : Model -> Html Msg
view { photos } =
    ol []
        <| List.map viewThumbnail photos


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


flickrGetPhotos =
    let
        url =
            Http.url "https://api.flickr.com/services/rest/"
                -- usually it's a good idea to hide api keys
                [ "api_key" => "edd9657c0af38e133a0416cc1815dac7"
                , "method" => "flickr.people.getPhotos"
                , "format" => "json"
                , "nojsoncallback" => "1"
                , "user_id" => "37993285@N06"
                , "safe_search" => "2"
                , "content_type" => "1"
                , "privacy_filter" => "1"
                , "per_page" => "500"
                , "extras" => "url_s"
                ]
    in
        Task.perform FetchFail FetchSucceed <| Http.get photosDecoder url


photosDecoder =
    let
        decodePhoto =
            decode Photo
                |> required "id" Decoder.string
                |> optional "title" Decoder.string ""
                |> required "url_s" Decoder.string
    in
        Decoder.at [ "photos", "photo" ]
            <| Decoder.list decodePhoto
