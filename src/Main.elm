module Main exposing (main)

import Browser exposing (Document)
import Element exposing (Element, padding, spacing)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import List.Extra as List
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
    = NoOp


type alias Model =
    { pageTitle : String
    }



--------------------------------------------------------------------------------
-- Setup -----------------------------------------------------------------------
--------------------------------------------------------------------------------


initModel : Model
initModel =
    { pageTitle = "Hello, World!" }


init : a -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
--------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



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
        , renderTable dummyLayerRenderer dummyData
        , renderTable
            (heatmapLayerRenderer
                { min = 10
                , max = 0
                , color = GrayscaleHeatmap
                }
            )
            dummyHeatmap
        ]


dummyData : Layer ()
dummyData =
    { width = 7
    , height = 4
    , data = List.repeat (7 * 4) ()
    }


dummyHeatmap : Layer Float
dummyHeatmap =
    { width = 3
    , height = 3
    , data = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
    }



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
        \data ->
            Svg.rect
                [ Svg.Attributes.width "100"
                , Svg.Attributes.height "100"
                , Svg.Attributes.fill "white"
                , Svg.Attributes.stroke "black"
                ]
                []
    }


type HeatmapColor
    = GrayscaleHeatmap


heatmapColor : HeatmapColor -> Float -> String
heatmapColor color intensity =
    case color of
        GrayscaleHeatmap ->
            let
                intensity255 =
                    String.fromFloat (255 * intensity)
            in
            "rgb(" ++ intensity255 ++ "," ++ intensity255 ++ "," ++ intensity255 ++ ")"


heatmapLayerRenderer : { min : Float, max : Float, color : HeatmapColor } -> LayerRenderer Float
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


renderTable : LayerRenderer a -> Layer a -> Element Msg
renderTable renderer data =
    let
        width =
            data.width * renderer.cellWidth

        height =
            data.height * renderer.cellHeight

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
        (Svg.svg attributes (renderTableSvg renderer data))


renderTableSvg : LayerRenderer a -> Layer a -> List (Svg Msg)
renderTableSvg renderer data =
    let
        xRange =
            List.range 0 (data.width - 1)

        yRange =
            List.range 0 (data.height - 1)
    in
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
