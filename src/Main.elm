module Main exposing (main)

import Browser exposing (Document)
import Element exposing (Element, padding, spacing)
import Element.Background as Background
import Element.Border as Border
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
    = GotGames (Result Http.Error (List GameType))
    | GotModels (Result Http.Error (List ModelDescription))
    | RequestImage GameState ImageRequest
    | GotImage (Result Http.Error GameWithImage)
    | RequestGameState GameType
    | SelectGameType GameType
    | SelectModel String
    | PostGameTurn TurnRequest
    | GotGameTurn GameType (Result Http.Error Value)
    | HideLayer String
    | ShowLayer String
    | InputPower String
    | InputTemperature String
    | InputExploration String
    | SetSelected Int Bool
    | SetGridWidth Int


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
    , lastAction : Int
    , allLayers : Set String
    , hiddenLayers : Set String
    , inputPower : String
    , inputTemperature : String
    , inputExploration : String
    , gridWidth : Int
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
    , selected : Bool
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


getSelectedGames : Model -> List GameWithImage
getSelectedGames model =
    model.gameStateList
        |> List.filter .selected


removeSelection : List GameWithImage -> List GameWithImage
removeSelection list =
    List.map (\s -> { s | selected = False }) list


copyLayerInformation : Image -> Model -> Model
copyLayerInformation image model =
    let
        newLayerNames =
            image.value.layers
                |> List.map Image.layerName
                |> Set.fromList
                |> Set.union model.allLayers
    in
    { model | allLayers = newLayerNames }



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
    , lastAction = -1
    , allLayers = Set.empty
    , hiddenLayers = Set.empty
    , inputPower = "500"
    , inputTemperature = "0.5"
    , inputExploration = "1.5"
    , gridWidth = 4
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
                        | gameStateList = List.append (removeSelection model.gameStateList) [ gameWithImage ]
                        , isLoadingNewState = False
                      }
                        |> copyLayerInformation gameWithImage.image
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

        SetSelected index isSelected ->
            ( { model
                | gameStateList = List.updateAt index (\s -> { s | selected = isSelected }) model.gameStateList
              }
            , Cmd.none
            )

        SetGridWidth newWidth ->
            ( { model | gridWidth = newWidth }, Cmd.none )


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
    model.gameStateList
        |> List.indexedMap gameButtonInSidebar
        |> Element.column [ Element.alignRight, Element.alignTop ]


gameButtonInSidebar : Int -> GameWithImage -> Element Msg
gameButtonInSidebar i state =
    let
        attrs =
            if state.selected then
                [ Background.color (Element.rgb255 200 200 200) ]

            else
                []
    in
    Element.el (List.append [ padding 10, Events.onClick (SetSelected i (not state.selected)) ] attrs)
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
            , Input.slider []
                { onChange = round >> SetGridWidth
                , label = Input.labelAbove [ Element.centerY ] (Element.text "Grid Width")
                , min = 1
                , max = 6
                , value = toFloat model.gridWidth
                , thumb = Input.defaultThumb
                , step = Just 1
                }
            ]
        , listOfGames model
        , listOfModels model
        , listOfLayers model
        , gameStateInformation model
        ]


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


listOfLayers : Model -> Element Msg
listOfLayers model =
    model.allLayers
        |> Set.toList
        |> List.sort
        |> List.map (layerName model)
        |> Element.row [ spacing 10 ]


oneGameWithImage : Model -> GameWithImage -> Element Msg
oneGameWithImage model gameWithImage =
    Element.column [ spacing 10 ]
        [ Element.text gameWithImage.gameState.gameType.typName
        , Element.el
            [ Events.onClick (RequestImage gameWithImage.gameState (buildImageRequest model gameWithImage.gameState.value)) ]
            (Element.text "Evaluate position")
        , imageViewInner model gameWithImage
        ]


gameStateInformation : Model -> Element Msg
gameStateInformation model =
    getSelectedGames model
        |> List.map (oneGameWithImage model)
        |> easyGrid model.gridWidth [ spacing 10 ]


imageViewInner : Model -> GameWithImage -> Element Msg
imageViewInner model gameWithImage =
    Element.el [] (prepareImage model gameWithImage.image |> renderMany |> Element.html)
        |> Element.map (actionToMsg gameWithImage)


actionToMsg : GameWithImage -> Int -> Msg
actionToMsg gameWithImage action =
    PostGameTurn
        { game = gameWithImage.gameState.value
        , action = action
        , gameType = gameWithImage.gameState.gameType
        }


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
        , expect = Http.expectJson (Result.map (\image -> { image = image, gameState = gameState, selected = True }) >> GotImage) Image.decode
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



--------------------------------------------------------------------------------
-- View Components -------------------------------------------------------------
--------------------------------------------------------------------------------
-- View components should not depend on any information that is specific to this
-- application. I am planing to move this whole block into a separate file when
-- all components that I have identified are moved into this block.


{-| Creates a grid with the given amount of columns. You can pass in a list of
attributes which will be applied to both the column and row element. Typically
you would pass in `[ spacing 5 ]` in here.
-}
easyGrid : Int -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
easyGrid columnCount attributes list =
    list
        |> List.greedyGroupsOf columnCount
        |> List.map (\group -> Element.row attributes group)
        |> Element.column attributes
