module Main exposing (main)

import Browser exposing (Document)
import Element exposing (Element, padding, spacing)
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Image exposing (Image, ImageLayer(..))
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Layer exposing (DisplayLayer, HeatmapColor(..), actionRenderer, heatmapLayerRenderer, renderMany, tokenRenderer)
import RemoteData exposing (WebData)
import Set exposing (Set)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = documentView
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = NoOp
    | GotGames (Result Http.Error (List GameType))
    | GotModels (Result Http.Error (List ModelDescription))
    | RequestImage ImageRequest
    | GotImage (Result Http.Error Image)
    | RequestGameState GameType
    | GotGameState GameType (Result Http.Error Value)
    | SelectGameType GameType
    | SelectModel String
    | PostGameTurn TurnRequest
    | GotGameTurn GameType (Result Http.Error Value)
    | HideLayer String
    | ShowLayer String
    | InputPower String
    | InputTemperature String
    | InputExploration String


type alias Model =
    { pageTitle : String
    , gameTypes : WebData (List GameType)
    , selectedGameType : Maybe GameType
    , models : WebData (List ModelDescription)
    , selectedModel : Maybe String
    , gameState : WebData GameState
    , image : WebData Image
    , lastAction : Int
    , hiddenLayers : Set String
    , inputPower : String
    , inputTemperature : String
    , inputExploration : String
    }


type alias ModelDescription =
    { name : String
    , game : String
    , features : List String
    , params : Int
    }


type alias GameType =
    { typName : String
    , params : List JuliaTypeParameter
    }


type alias GameState =
    { gameType : GameType
    , value : Value
    }


type JuliaTypeParameter
    = JuliaInt64 Int


juliaTypeToString param =
    case param of
        JuliaInt64 value ->
            String.fromInt value



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
    , hiddenLayers = Set.empty
    , inputPower = "500"
    , inputTemperature = "0.5"
    , inputExploration = "1.5"
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
        NoOp ->
            ( model, Cmd.none )

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

        GotGameState gameType response ->
            ( { model
                | gameState =
                    RemoteData.fromResult response
                        |> RemoteData.map (\value -> { value = value, gameType = gameType })
              }
            , Cmd.none
            )

        SelectGameType newType ->
            ( { model
                | selectedGameType = Just newType
                , gameState = RemoteData.NotAsked
              }
            , Cmd.none
            )

        SelectModel newModel ->
            ( { model | selectedModel = Just newModel }, Cmd.none )

        PostGameTurn turnRequest ->
            ( { model | lastAction = turnRequest.action }, postTurn turnRequest )

        HideLayer name ->
            ( { model | hiddenLayers = Set.insert name model.hiddenLayers }, Cmd.none )

        ShowLayer name ->
            ( { model | hiddenLayers = Set.remove name model.hiddenLayers }, Cmd.none )

        GotGameTurn gameType response ->
            ( { model
                | gameState =
                    RemoteData.fromResult response
                        |> RemoteData.map (\value -> { value = value, gameType = gameType })
              }
            , case response of
                Ok value ->
                    getImage (buildImageRequest model value)

                _ ->
                    Cmd.none
            )

        InputPower text ->
            ( { model | inputPower = text }, Cmd.none )

        InputTemperature text ->
            ( { model | inputTemperature = text }, Cmd.none )

        InputExploration text ->
            ( { model | inputExploration = text }, Cmd.none )


buildImageRequest : Model -> Value -> ImageRequest
buildImageRequest model value =
    { game = value
    , model = model.selectedModel |> Maybe.withDefault "rollout"
    , power = String.toInt model.inputPower |> Maybe.withDefault 500
    , temperature = String.toFloat model.inputTemperature |> Maybe.withDefault 0.5
    , exploration = String.toFloat model.inputExploration |> Maybe.withDefault 1.5
    }



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
        [ Element.row [ spacing 10 ]
            [ Input.text []
                { onChange = InputPower
                , text = model.inputPower
                , placeholder = Nothing
                , label = Input.labelAbove [ Element.centerY ] (Element.text "Power")
                }
            , Input.text []
                { onChange = InputTemperature
                , text = model.inputTemperature
                , placeholder = Nothing
                , label = Input.labelAbove [ Element.centerY ] (Element.text "Temperature")
                }
            , Input.text []
                { onChange = InputExploration
                , text = model.inputExploration
                , placeholder = Nothing
                , label = Input.labelAbove [ Element.centerY ] (Element.text "Exploration")
                }
            ]
        , listOfGames model
        , listOfModels model
        , gameStateInformation model model.gameState
        , Element.text ("Last action: " ++ String.fromInt model.lastAction)
        , imageView model
        ]


gameStateInformation : Model -> WebData GameState -> Element Msg
gameStateInformation model value =
    webDataEasyWrapper
        (\game ->
            Element.row [ spacing 10 ]
                [ Element.text "We have a game state."
                , Element.text game.gameType.typName
                , Element.el
                    [ Events.onClick (RequestImage (buildImageRequest model game.value)) ]
                    (Element.text "Evaluate position")
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
                    (List.map
                        (chooseButton .typName SelectGameType model.selectedGameType)
                        list
                    )
            )
            model.gameTypes
        , case model.selectedGameType of
            Just gameType ->
                Element.el [ Events.onClick (RequestGameState gameType) ] (Element.text "Create Game")

            Nothing ->
                Element.none
        ]


listOfModels : Model -> Element Msg
listOfModels model =
    webDataEasyWrapper
        (\list ->
            Element.row [ spacing 10 ]
                (List.map
                    (\m -> chooseButton (\x -> x) SelectModel model.selectedModel m.name)
                    list
                )
        )
        model.models


chooseButton : (a -> String) -> (a -> Msg) -> Maybe a -> a -> Element Msg
chooseButton writer event current label =
    if current == Just label then
        Element.el [ Font.heavy ] (Element.text (writer label))

    else
        Element.el [ Events.onClick (event label) ] (Element.text (writer label))


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
    webDataEasyWrapper (imageViewInner model) model.image


imageViewInner : Model -> Image -> Element Msg
imageViewInner model image =
    Element.row [ spacing 10 ]
        [ Element.el [] (prepareImage model image |> renderMany |> Element.html)
            |> Element.map (actionToMsg model)
        , Element.el [ Element.alignTop ] (Element.text "Layers")
        , image.value.layers
            |> List.map Image.layerName
            |> List.map (layerName model)
            |> Element.column [ spacing 10 ]
        ]


actionToMsg : Model -> Int -> Msg
actionToMsg model action =
    RemoteData.unwrap NoOp
        (\gameState ->
            PostGameTurn
                { game = gameState.value
                , action = action
                , gameType = gameState.gameType
                }
        )
        model.gameState


layerName : Model -> String -> Element Msg
layerName model name =
    if Set.member name model.hiddenLayers then
        Element.el [ Font.strike, Events.onClick (ShowLayer name) ] (Element.text name)

    else
        Element.el [ Events.onClick (HideLayer name) ] (Element.text name)


prepareImage : Model -> Image -> List (DisplayLayer Int)
prepareImage model image =
    image.value.layers
        |> List.filter (\layer -> not (Set.member (Image.layerName layer) model.hiddenLayers))
        |> List.map (prepareImageLayer image)


prepareImageLayer : Image -> ImageLayer -> DisplayLayer Int
prepareImageLayer image layer =
    case layer of
        ImageLayerHeatmap value ->
            Layer.DisplayLayerFloat
                (heatmapLayerRenderer
                    { min = value.min
                    , max = value.max
                    , color = BlueHeatmap
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
                                Just i

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


decodeGameType : Decode.Decoder GameType
decodeGameType =
    Decode.map2 GameType
        (Decode.field "typ" Decode.string)
        (Decode.field "params" (Decode.list decodeJuliaTypeParameter))


fromStringJuliaTypeParameter : String -> Decode.Decoder JuliaTypeParameter
fromStringJuliaTypeParameter string =
    case string of
        "Int64" ->
            Decode.succeed (JuliaInt64 5)

        _ ->
            Decode.fail ("Not valid pattern for decoder to JuliaTypeParameter. Pattern: " ++ string)


decodeJuliaTypeParameter : Decode.Decoder JuliaTypeParameter
decodeJuliaTypeParameter =
    Decode.string |> Decode.andThen fromStringJuliaTypeParameter


getGames : Cmd Msg
getGames =
    Http.get
        { url = "/api/games"
        , expect = Http.expectJson GotGames (Decode.list decodeGameType)
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


getNewGame : GameType -> Cmd Msg
getNewGame gameType =
    let
        url =
            "/api/create/"
                ++ gameType.typName
                ++ (gameType.params
                        |> List.map (\p -> "/" ++ juliaTypeToString p)
                        |> String.concat
                   )
    in
    Http.get
        { url = url
        , expect = Http.expectJson (GotGameState gameType) Decode.value
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
        , expect = Http.expectJson GotImage Image.decode
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


type alias TurnRequest =
    { game : Value
    , action : Int
    , gameType : GameType
    }


encodeTurnRequest : TurnRequest -> Value
encodeTurnRequest record =
    Encode.object
        [ ( "game", record.game )
        , ( "action", Encode.int <| record.action )
        ]


postTurn : TurnRequest -> Cmd Msg
postTurn config =
    Http.post
        { url = "/api/turn"
        , body = Http.jsonBody (encodeTurnRequest config)
        , expect = Http.expectJson (GotGameTurn config.gameType) Decode.value
        }
