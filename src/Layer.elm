module Layer exposing
    ( ActionRendererConfig
    , DisplayLayer(..)
    , HeatmapColor(..)
    , HeatmapRendererConfig
    , Layer
    , LayerRenderer
    , TokenRendererConfig
    , actionRenderer
    , heatmapLayerRenderer
    , renderMany
    , renderOne
    , tokenRenderer
    )

{-| A module that encapsulates rendering data layers into svg images.

The data is held in `Layer a` objects and you can use a matching
`LayerRenderer a msg` to turn this into a svg.

-}

import List.Extra as List
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


{-| The data we get from the server is usually interpreted as a rectangular grid.
-}
type alias Layer a =
    { width : Int
    , height : Int
    , data : List a
    }


{-| Gives an instruction how to turn a single cell into a svg element and how
much space to reserve for each element.
-}
type alias LayerRenderer a msg =
    { cellRenderer : a -> Svg msg
    , cellWidth : Int
    , cellHeight : Int
    }


renderOne : LayerRenderer a msg -> Layer a -> Svg msg
renderOne renderer data =
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


{-| When I want to mix layers of different types, Elm needs to know which type
each layer has. I think this corresponds to a manual implementation of dynamic
dispatch when rendering the layer.
-}
type DisplayLayer msg
    = DisplayLayerFloat (LayerRenderer Float msg) (Layer Float)
    | DisplayLayerString (LayerRenderer String msg) (Layer String)
    | DisplayLayerInt (LayerRenderer Int msg) (Layer Int)


renderDisplayLayer : DisplayLayer msg -> Svg msg
renderDisplayLayer layer =
    case layer of
        DisplayLayerFloat renderer data ->
            renderOne renderer data

        DisplayLayerString renderer data ->
            renderOne renderer data

        DisplayLayerInt renderer data ->
            renderOne renderer data


renderMany : List (DisplayLayer msg) -> Svg msg
renderMany displayLayer =
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
    Svg.svg attributes (List.map renderDisplayLayer displayLayer)


widthDisplayLayer : DisplayLayer msg -> Int
widthDisplayLayer layer =
    case layer of
        DisplayLayerFloat renderer data ->
            data.width * renderer.cellWidth

        DisplayLayerString renderer data ->
            data.width * renderer.cellWidth

        DisplayLayerInt renderer data ->
            data.width * renderer.cellWidth


heightDisplayLayer : DisplayLayer msg -> Int
heightDisplayLayer layer =
    case layer of
        DisplayLayerFloat renderer data ->
            data.height * renderer.cellWidth

        DisplayLayerString renderer data ->
            data.height * renderer.cellWidth

        DisplayLayerInt renderer data ->
            data.height * renderer.cellWidth



--------------------------------------------------------------------------------
-- Default Layer Renderer Definitions ------------------------------------------
--------------------------------------------------------------------------------


type alias TokenRendererConfig =
    { color : String
    }


tokenRenderer : TokenRendererConfig -> LayerRenderer String a
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


heatmapLayerRenderer : HeatmapRendererConfig -> LayerRenderer Float msg
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


type alias ActionRendererConfig a msg =
    { event : a -> Maybe msg
    }


actionRenderer : ActionRendererConfig a msg -> LayerRenderer a msg
actionRenderer config =
    { cellWidth = 100
    , cellHeight = 100
    , cellRenderer =
        \value ->
            let
                attributes =
                    [ Svg.Attributes.width "100"
                    , Svg.Attributes.height "100"
                    , Svg.Attributes.fill "rgba(0,0,0,0)"
                    , Svg.Attributes.stroke "black"
                    ]
            in
            case config.event value of
                Just msg ->
                    Svg.rect (Svg.Events.onClick msg :: attributes) []

                Nothing ->
                    Svg.g [] []
    }
