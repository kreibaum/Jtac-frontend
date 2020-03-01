module Main exposing (main)

import Browser exposing (Document)
import Element exposing (Element, padding, spacing)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Image exposing (Image, ImageLayer(..))
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Layer exposing (DisplayLayer, HeatmapColor(..), actionRenderer, heatmapLayerRenderer, renderMany, tokenRenderer)
import List.Extra as List
import RemoteData exposing (WebData)
import Set exposing (Set)


main : Program () OuterModel Msg
main =
    Browser.document
        { init = initOuter
        , view = documentView
        , update = updateOuter
        , subscriptions = subscriptions
        }


type Msg
    = NoOp
    | GotGames (Result Http.Error (List GameType))
    | GotModels (Result Http.Error (List ModelDescription))
    | RequestImage GameState ImageRequest
    | GotImage (Result Http.Error GameWithImage)
    | RequestGameState GameType
      --| GotGameState GameType (Result Http.Error Value)
    | SelectGameType GameType
    | SelectModel String
    | PostGameTurn TurnRequest
    | GotGameTurn GameType (Result Http.Error Value)
    | HideLayer String
    | ShowLayer String
    | InputPower String
    | InputTemperature String
    | InputExploration String
    | SetSelected Int


type OuterModel
    = LoadingStaticInfo LoadingModel
    | WebsiteReady Model
    | ErrorWhileLoading String


type alias LoadingModel =
    { gameTypes : WebData (List GameType)
    , models : WebData (List ModelDescription)
    }


type alias Model =
    { gameTypes : List GameType
    , selectedGameType : Maybe GameType
    , models : List ModelDescription
    , selectedModel : Maybe String
    , gameStateList : List GameWithImage
    , isLoadingNewState : Bool
    , errorLog : List String
    , selectedIndex : Int
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


type alias GameWithImage =
    { gameState : GameState
    , image : Image
    }


type JuliaTypeParameter
    = JuliaInt64 Int


juliaTypeToString : JuliaTypeParameter -> String
juliaTypeToString param =
    case param of
        JuliaInt64 value ->
            String.fromInt value



--------------------------------------------------------------------------------
-- Optional Getters ------------------------------------------------------------
--------------------------------------------------------------------------------


getSelectedGame : Model -> Maybe GameWithImage
getSelectedGame model =
    List.getAt model.selectedIndex model.gameStateList



--------------------------------------------------------------------------------
-- Setup -----------------------------------------------------------------------
--------------------------------------------------------------------------------


{-| Takes a loading model and tries to upgrade it to a regular model.
-}
tryInit : LoadingModel -> OuterModel
tryInit loadingModel =
    let
        readyModel =
            RemoteData.map2 initModel loadingModel.gameTypes loadingModel.models
    in
    case readyModel of
        RemoteData.Success model ->
            WebsiteReady model

        RemoteData.Loading ->
            LoadingStaticInfo loadingModel

        RemoteData.Failure _ ->
            ErrorWhileLoading "Error while loading static data from server."

        RemoteData.NotAsked ->
            ErrorWhileLoading "Init function did not ask for data."


initModel : List GameType -> List ModelDescription -> Model
initModel gameTypes models =
    { gameTypes = gameTypes
    , selectedGameType = Nothing
    , models = models
    , selectedModel = Nothing
    , gameStateList = []
    , isLoadingNewState = False
    , errorLog = []
    , selectedIndex = -1
    , lastAction = -1
    , hiddenLayers = Set.empty
    , inputPower = "500"
    , inputTemperature = "0.5"
    , inputExploration = "1.5"
    }


initOuter : a -> ( OuterModel, Cmd Msg )
initOuter _ =
    ( LoadingStaticInfo
        { gameTypes = RemoteData.Loading
        , models = RemoteData.Loading
        }
    , Cmd.batch [ getGames, getModels ]
    )


subscriptions : OuterModel -> Sub Msg
subscriptions _ =
    Sub.none



--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
--------------------------------------------------------------------------------


{-| Wrapper around the update function where we handle the case that the website is not fully loaded yet.
-}
updateOuter : Msg -> OuterModel -> ( OuterModel, Cmd Msg )
updateOuter msg outerModel =
    case outerModel of
        LoadingStaticInfo loadingModel ->
            case msg of
                GotGames response ->
                    ( tryInit { loadingModel | gameTypes = RemoteData.fromResult response }, Cmd.none )

                GotModels response ->
                    ( tryInit { loadingModel | models = RemoteData.fromResult response }, Cmd.none )

                _ ->
                    ( ErrorWhileLoading "Unexpected Message recieved", Cmd.none )

        WebsiteReady model ->
            let
                ( newModel, cmd ) =
                    update msg model
            in
            ( WebsiteReady newModel, cmd )

        ErrorWhileLoading _ ->
            ( outerModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotGames _ ->
            Debug.todo "Handled earlier, need to refactor message type to remove this branch."

        GotModels _ ->
            Debug.todo "Handled earlier, need to refactor message type to remove this branch."

        RequestImage gameState request ->
            ( { model | isLoadingNewState = True }, getImage gameState request )

        GotImage response ->
            case response of
                Ok gameWithImage ->
                    ( { model
                        | gameStateList = List.append model.gameStateList [ gameWithImage ]
                        , selectedIndex = List.length model.gameStateList
                        , isLoadingNewState = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | errorLog = List.append model.errorLog [ "Error while loading image" ] }, Cmd.none )

        RequestGameState gameType ->
            ( { model | isLoadingNewState = True }, getNewGame gameType )

        SelectGameType newType ->
            ( { model
                | selectedGameType = Just newType
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
            let
                gameState value =
                    { value = value, gameType = gameType }
            in
            case response of
                Ok value ->
                    ( model
                    , getImage (gameState value) (buildImageRequest model value)
                    )

                Err _ ->
                    ( { model | errorLog = List.append model.errorLog [ "Error while loading game" ] }, Cmd.none )

        InputPower text ->
            ( { model | inputPower = text }, Cmd.none )

        InputTemperature text ->
            ( { model | inputTemperature = text }, Cmd.none )

        InputExploration text ->
            ( { model | inputExploration = text }, Cmd.none )

        SetSelected index ->
            ( { model
                | selectedIndex = index
              }
            , Cmd.none
            )


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


documentView : OuterModel -> Document Msg
documentView outerModel =
    { title = "Jtac analysis frontend"
    , body = [ elmUiLayout outerModel ]
    }


elmUiLayout : OuterModel -> Html Msg
elmUiLayout outerModel =
    Element.layout []
        (case outerModel of
            LoadingStaticInfo _ ->
                Element.text "Loading ..."

            WebsiteReady model ->
                view model

            ErrorWhileLoading errorMessage ->
                Element.column []
                    [ Element.text "Error:"
                    , Element.text errorMessage
                    ]
        )


view : Model -> Element Msg
view model =
    Element.row [ Element.width Element.fill ]
        [ centerView model
        , sidebarView model
        ]


sidebarView : Model -> Element Msg
sidebarView model =
    Element.column [ Element.alignRight, Element.alignTop ]
        (model.gameStateList
            |> List.indexedMap (gameButtonInSidebar model)
        )


gameButtonInSidebar : Model -> Int -> GameWithImage -> Element Msg
gameButtonInSidebar model i state =
    let
        attrs =
            if model.selectedIndex == i then
                [ Background.color (Element.rgb255 200 200 200) ]

            else
                []
    in
    Element.el (List.append [ padding 10, Events.onClick (SetSelected i) ] attrs)
        (Element.text state.gameState.gameType.typName)


centerView : Model -> Element Msg
centerView model =
    Element.column [ padding 10, spacing 10, Element.width Element.fill ]
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
        , gameStateInformation model
        , Element.text ("Last action: " ++ String.fromInt model.lastAction)
        , imageView model
        ]


gameStateInformation : Model -> Element Msg
gameStateInformation model =
    case getSelectedGame model of
        Just gameWithImage ->
            Element.row [ spacing 10 ]
                [ Element.text "We have a game state."
                , Element.text gameWithImage.gameState.gameType.typName
                , Element.el
                    [ Events.onClick (RequestImage gameWithImage.gameState (buildImageRequest model gameWithImage.gameState.value)) ]
                    (Element.text "Evaluate position")
                ]

        Nothing ->
            Element.text "No state selected."


listOfGames : Model -> Element Msg
listOfGames model =
    Element.row [ spacing 10 ]
        [ Element.text "Supported Game Types:"
        , Element.row [ spacing 10 ]
            (List.map
                (chooseButton .typName SelectGameType model.selectedGameType)
                model.gameTypes
            )
        , case model.selectedGameType of
            Just gameType ->
                Element.el [ Events.onClick (RequestGameState gameType) ] (Element.text "Create Game")

            Nothing ->
                Element.none
        ]


listOfModels : Model -> Element Msg
listOfModels model =
    Element.row [ spacing 10 ]
        (List.map
            (\m -> chooseButton (\x -> x) SelectModel model.selectedModel m.name)
            model.models
        )


chooseButton : (a -> String) -> (a -> Msg) -> Maybe a -> a -> Element Msg
chooseButton writer event current label =
    if current == Just label then
        Element.el [ Font.heavy ] (Element.text (writer label))

    else
        Element.el [ Events.onClick (event label) ] (Element.text (writer label))


imageView : Model -> Element Msg
imageView model =
    case getSelectedGame model of
        Just gameWithImage ->
            imageViewInner model gameWithImage.image

        Nothing ->
            Element.text "No image selected."


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
    case getSelectedGame model of
        Just gameWithImage ->
            PostGameTurn
                { game = gameWithImage.gameState.value
                , action = action
                , gameType = gameWithImage.gameState.gameType
                }

        Nothing ->
            NoOp


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

        ImageLayerLines value ->
            Layer.DisplayLayerLines
                { data = value.data
                , name = value.name
                , width = image.value.width
                , height = image.value.height
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
        , expect = Http.expectJson (GotGameTurn gameType) Decode.value
        }


type alias ImageRequest =
    { game : Value
    , model : String
    , power : Int
    , temperature : Float
    , exploration : Float
    }


getImage : GameState -> ImageRequest -> Cmd Msg
getImage gameState config =
    Http.post
        { url = "/api/apply/visual"
        , body = Http.jsonBody (encodeImageRequest config)
        , expect = Http.expectJson (Result.map (\image -> { image = image, gameState = gameState }) >> GotImage) Image.decode
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
