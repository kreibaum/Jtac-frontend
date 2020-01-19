module Main exposing (main)

import Browser exposing (Document)
import Element exposing (Element, padding, spacing)
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import Http
import Image exposing (Image, ImageLayer(..))
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Layer exposing (DisplayLayer, HeatmapColor(..), actionRenderer, heatmapLayerRenderer, renderMany, tokenRenderer)
import RemoteData exposing (WebData)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = documentView
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = GotGames (Result Http.Error (List String))
    | GotModels (Result Http.Error (List ModelDescription))
    | RequestImage ImageRequest
    | GotImage (Result Http.Error Image)
    | RequestGameState String
    | GotGameState (Result Http.Error Value)
    | SelectGameType String
    | SelectModel String
    | SetLastAction Int


type alias Model =
    { pageTitle : String
    , gameTypes : WebData (List String)
    , selectedGameType : Maybe String
    , models : WebData (List ModelDescription)
    , selectedModel : Maybe String
    , gameState : WebData Value
    , image : WebData Image
    , lastAction : Int
    }


type alias ModelDescription =
    { name : String
    , game : String
    , features : List String
    , params : Int
    }



--------------------------------------------------------------------------------
-- Setup -----------------------------------------------------------------------
--------------------------------------------------------------------------------


initModel : Model
initModel =
    { pageTitle = "Hello, World!"
    , gameTypes = RemoteData.Loading
    , selectedGameType = Nothing
    , models = RemoteData.Loading
    , selectedModel = Nothing
    , gameState = RemoteData.NotAsked
    , image = RemoteData.Loading
    , lastAction = -1
    }


init : a -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.batch [ getGames, getModels, getDummyImage ] )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
--------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGames response ->
            ( { model | gameTypes = RemoteData.fromResult response }, Cmd.none )

        GotModels response ->
            ( { model | models = RemoteData.fromResult response }, Cmd.none )

        RequestImage request ->
            ( { model | image = RemoteData.Loading }, getImage request )

        GotImage response ->
            ( { model | image = RemoteData.fromResult response }, Cmd.none )

        RequestGameState gameType ->
            ( { model | gameState = RemoteData.Loading }, getNewGame gameType )

        GotGameState response ->
            ( { model | gameState = RemoteData.fromResult response }, Cmd.none )

        SelectGameType newType ->
            ( { model
                | selectedGameType = Just newType
                , gameState = RemoteData.NotAsked
              }
            , Cmd.none
            )

        SelectModel newModel ->
            ( { model | selectedModel = Just newModel }, Cmd.none )

        SetLastAction newAction ->
            ( { model | lastAction = newAction }, Cmd.none )



--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
--------------------------------------------------------------------------------


documentView : Model -> Document Msg
documentView model =
    { title = model.pageTitle
    , body = [ elmUiLayout model ]
    }


elmUiLayout : Model -> Html Msg
elmUiLayout model =
    Element.layout [] (view model)


view : Model -> Element Msg
view model =
    Element.column [ padding 10, spacing 10 ]
        [ Element.text "Exploring states in Jtac.jl"
        , Element.text "Written in Elm."
        , listOfGames model
        , listOfModels model
        , gameStateInformation model.gameState
        , Element.text ("Last action: " ++ String.fromInt model.lastAction)
        , imageView model
        ]


gameStateInformation : WebData Value -> Element Msg
gameStateInformation value =
    webDataEasyWrapper
        (\game ->
            Element.row [ spacing 10 ]
                [ Element.text "We have a game state."
                , Element.el
                    [ Events.onClick
                        (RequestImage
                            { game = game
                            , model = "rollout"
                            , power = 50
                            , temperature = 0.5
                            , exploration = 0.7
                            }
                        )
                    ]
                    (Element.text "Try to crash")
                ]
        )
        value


listOfGames : Model -> Element Msg
listOfGames model =
    Element.row [ spacing 10 ]
        [ Element.text "Supported Game Types:"
        , webDataEasyWrapper
            (\list ->
                Element.row [ spacing 10 ]
                    (List.map (chooseButton SelectGameType model.selectedGameType) list)
            )
            model.gameTypes
        , Element.el [ Events.onClick (RequestGameState "TicTacToe") ] (Element.text "Create Game")
        ]


listOfModels : Model -> Element Msg
listOfModels model =
    webDataEasyWrapper
        (\list ->
            Element.row [ spacing 10 ]
                (List.map
                    (\m -> chooseButton SelectModel model.selectedModel m.name)
                    list
                )
        )
        model.models


chooseButton : (String -> Msg) -> Maybe String -> String -> Element Msg
chooseButton event current label =
    if current == Just label then
        Element.el [ Font.heavy ] (Element.text label)

    else
        Element.el [ Events.onClick (event label) ] (Element.text label)


webDataEasyWrapper : (a -> Element Msg) -> WebData a -> Element Msg
webDataEasyWrapper ifPresent data =
    case data of
        RemoteData.Success success ->
            ifPresent success

        RemoteData.Failure _ ->
            Element.text "Http error"

        RemoteData.Loading ->
            Element.text "Loading"

        RemoteData.NotAsked ->
            Element.text "Not asked"


imageView : Model -> Element Msg
imageView model =
    webDataEasyWrapper
        (\image ->
            Element.row [ spacing 10 ]
                [ prepareImage image |> renderMany |> Element.html
                ]
        )
        model.image


prepareImage : Image -> List (DisplayLayer Msg)
prepareImage image =
    image.value.layers
        |> List.map (prepareImageLayer image)


prepareImageLayer : Image -> ImageLayer -> DisplayLayer Msg
prepareImageLayer image layer =
    case layer of
        ImageLayerHeatmap value ->
            Layer.DisplayLayerFloat
                (heatmapLayerRenderer
                    { min = value.min
                    , max = value.max
                    , color = GrayscaleHeatmap
                    }
                )
                { width = image.value.width
                , height = image.value.height
                , data = value.data
                }

        ImageLayerTokens value ->
            Layer.DisplayLayerString
                (tokenRenderer { color = "black" })
                { width = image.value.width
                , height = image.value.height
                , data = value.data
                }

        ImageLayerActions value ->
            Layer.DisplayLayerInt
                (actionRenderer
                    { event =
                        \i ->
                            if i > 0 then
                                Just (SetLastAction i)

                            else
                                Nothing
                    }
                )
                { width = image.value.width
                , height = image.value.height
                , data = value.data
                }



--------------------------------------------------------------------------------
-- REST Api --------------------------------------------------------------------
--------------------------------------------------------------------------------


getGames : Cmd Msg
getGames =
    Http.get
        { url = "/api/games"
        , expect = Http.expectJson GotGames (Decode.list Decode.string)
        }


getModels : Cmd Msg
getModels =
    Http.get
        { url = "/api/models"
        , expect = Http.expectJson GotModels (Decode.list decodeModelDescription)
        }


decodeModelDescription : Decode.Decoder ModelDescription
decodeModelDescription =
    Decode.map4 ModelDescription
        (Decode.field "name" Decode.string)
        (Decode.field "game" Decode.string)
        (Decode.field "features" (Decode.list Decode.string))
        (Decode.field "params" Decode.int)


getNewGame : String -> Cmd Msg
getNewGame gameType =
    Http.get
        { url = "/api/create/" ++ gameType
        , expect = Http.expectJson GotGameState Decode.value
        }


type alias ImageRequest =
    { game : Value
    , model : String
    , power : Int
    , temperature : Float
    , exploration : Float
    }


getImage : ImageRequest -> Cmd Msg
getImage config =
    Http.post
        { url = "/api/apply/visual"
        , body = Http.jsonBody (encodeImageRequest config)
        , expect = Http.expectJson GotGameState Decode.value
        }


encodeImageRequest : ImageRequest -> Value
encodeImageRequest record =
    Encode.object
        [ ( "game", record.game )
        , ( "model", Encode.string <| record.model )
        , ( "power", Encode.int <| record.power )
        , ( "temperature", Encode.float <| record.temperature )
        , ( "exploration", Encode.float <| record.exploration )
        ]


getDummyImage : Cmd Msg
getDummyImage =
    Http.get
        { url = "/api/dummyimage"
        , expect = Http.expectJson GotImage Image.decode
        }
