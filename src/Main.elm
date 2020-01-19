module Main exposing (main)

import Browser exposing (Document)
import Element exposing (Element, padding, spacing)
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Value)
import List.Extra as List
import RemoteData exposing (WebData)
import Svg exposing (Svg)
import Svg.Attributes


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
    | SelectGameType String
    | SelectModel String


type alias Model =
    { pageTitle : String
    , gameTypes : WebData (List String)
    , selectedGameType : Maybe String
    , models : WebData (List ModelDescription)
    , selectedModel : Maybe String
    , gameState : Maybe Value
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
    , gameState = Nothing
    }


init : a -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.batch [ getGames, getModels ] )


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

        SelectGameType newType ->
            ( { model | selectedGameType = Just newType, gameState = Nothing }, Cmd.none )

        SelectModel newModel ->
            ( { model | selectedModel = Just newModel }, Cmd.none )



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
        , renderTable
            [ DisplayLayerFloat
                (heatmapLayerRenderer
                    { min = 10
                    , max = 0
                    , color = GrayscaleHeatmap
                    }
                )
                dummyHeatmap
            , DisplayLayerString
                (tokenRenderer { color = "red" })
                dummyTokenData
            ]
        ]


gameStateInformation : Maybe Value -> Element a
gameStateInformation value =
    case value of
        Just _ ->
            Element.text "We have a game state."

        Nothing ->
            Element.text "We don't have a game state."


dummyHeatmap : Layer Float
dummyHeatmap =
    { width = 3
    , height = 3
    , data = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
    }


dummyTokenData : Layer String
dummyTokenData =
    { width = 3
    , height = 3
    , data = [ "A", "B", "C", "D", "E", "F", "ℝ", "O", "X" ]
    }


listOfGames : Model -> Element Msg
listOfGames model =
    webDataEasyWrapper
        (\list ->
            Element.row [ spacing 10 ]
                (List.map (chooseButton SelectGameType model.selectedGameType) list)
        )
        model.gameTypes


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



--------------------------------------------------------------------------------
-- Default Layer Renderer Definitions ------------------------------------------
--------------------------------------------------------------------------------


{-| A layer that just renders empty squares. Can be used to create a grid.
-}
dummyLayerRenderer : LayerRenderer a
dummyLayerRenderer =
    { cellWidth = 100
    , cellHeight = 100
    , cellRenderer =
        \_ ->
            Svg.rect
                [ Svg.Attributes.width "100"
                , Svg.Attributes.height "100"
                , Svg.Attributes.fill "white"
                , Svg.Attributes.stroke "black"
                ]
                []
    }


type alias HeatmapRendererConfig =
    { min : Float
    , max : Float
    , color : HeatmapColor
    }


type HeatmapColor
    = GrayscaleHeatmap
    | BlueHeatmap


heatmapColor : HeatmapColor -> Float -> String
heatmapColor color intensity =
    case color of
        GrayscaleHeatmap ->
            let
                intensity255 =
                    String.fromFloat (255 * intensity)
            in
            "rgb(" ++ intensity255 ++ "," ++ intensity255 ++ "," ++ intensity255 ++ ")"

        BlueHeatmap ->
            let
                intensity255 =
                    String.fromFloat (255 * intensity)
            in
            "rgb(0, 0," ++ intensity255 ++ ")"


heatmapLayerRenderer : HeatmapRendererConfig -> LayerRenderer Float
heatmapLayerRenderer config =
    let
        intensity value =
            (value - config.min)
                / (config.max - config.min)
                |> max 0
                |> min 1
    in
    { cellWidth = 100
    , cellHeight = 100
    , cellRenderer =
        \data ->
            Svg.rect
                [ Svg.Attributes.width "100"
                , Svg.Attributes.height "100"
                , Svg.Attributes.fill (heatmapColor config.color (intensity data))
                ]
                []
    }


type alias TokenRendererConfig =
    { color : String
    }


tokenRenderer : TokenRendererConfig -> LayerRenderer String
tokenRenderer config =
    { cellWidth = 100
    , cellHeight = 100
    , cellRenderer =
        \data ->
            Svg.text_
                [ Svg.Attributes.textAnchor "middle"
                , Svg.Attributes.transform "translate(50, 85)"
                , Svg.Attributes.fontSize "90px"
                , Svg.Attributes.fill config.color
                ]
                [ Svg.text data
                ]
    }



--------------------------------------------------------------------------------
-- Svg Tables ------------------------------------------------------------------
--------------------------------------------------------------------------------


{-| The data we get from the server is usually interpreted as a rectangular grid.
-}
type alias Layer a =
    { width : Int
    , height : Int
    , data : List a
    }


type alias LayerRenderer a =
    { cellRenderer : a -> Svg Msg
    , cellWidth : Int
    , cellHeight : Int
    }


type DisplayLayer
    = DisplayLayerFloat (LayerRenderer Float) (Layer Float)
    | DisplayLayerString (LayerRenderer String) (Layer String)


renderTable : List DisplayLayer -> Element Msg
renderTable displayLayer =
    let
        width =
            displayLayer
                |> List.head
                |> Maybe.map widthDisplayLayer
                |> Maybe.withDefault 0

        height =
            displayLayer
                |> List.head
                |> Maybe.map heightDisplayLayer
                |> Maybe.withDefault 0

        viewBox =
            String.join
                " "
                [ "0"
                , "0"
                , String.fromInt width
                , String.fromInt height
                ]
                |> Svg.Attributes.viewBox

        attributes =
            [ Svg.Attributes.width <| String.fromInt width
            , Svg.Attributes.height <| String.fromInt height
            , viewBox
            ]
    in
    Element.html
        (Svg.svg attributes (List.map renderDisplayLayer displayLayer))


renderTableSvg : LayerRenderer a -> Layer a -> Svg Msg
renderTableSvg renderer data =
    List.map
        (\info ->
            Svg.g
                [ Svg.Attributes.transform
                    ("translate("
                        ++ String.fromInt (renderer.cellWidth * info.x)
                        ++ ","
                        ++ String.fromInt (renderer.cellHeight * info.y)
                        ++ ")"
                    )
                ]
                [ renderer.cellRenderer info.data ]
        )
        (indexedData data)
        |> Svg.g []


type alias CellRenderInfo a =
    { x : Int
    , y : Int
    , data : a
    }


indexedData : Layer a -> List (CellRenderInfo a)
indexedData data =
    let
        xRange =
            List.range 0 (data.width - 1)

        yRange =
            List.range 0 (data.height - 1)
    in
    List.map
        (\x ->
            List.filterMap
                (\y ->
                    List.getAt (x + data.width * y) data.data
                        |> Maybe.map (\entry -> { x = x, y = y, data = entry })
                )
                yRange
        )
        xRange
        |> List.concat


renderDisplayLayer : DisplayLayer -> Svg Msg
renderDisplayLayer layer =
    case layer of
        DisplayLayerFloat renderer data ->
            renderTableSvg renderer data

        DisplayLayerString renderer data ->
            renderTableSvg renderer data


widthDisplayLayer : DisplayLayer -> Int
widthDisplayLayer layer =
    case layer of
        DisplayLayerFloat renderer data ->
            data.width * renderer.cellWidth

        DisplayLayerString renderer data ->
            data.width * renderer.cellWidth


heightDisplayLayer : DisplayLayer -> Int
heightDisplayLayer layer =
    case layer of
        DisplayLayerFloat renderer data ->
            data.height * renderer.cellWidth

        DisplayLayerString renderer data ->
            data.height * renderer.cellWidth



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
