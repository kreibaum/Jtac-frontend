module Image exposing
    ( ActionsValue
    , Float2D
    , HeatmapValue
    , Image
    , ImageLayer(..)
    , ImageValue
    , LineSegment
    , LinesValue
    , TokensValue
    , decode
    , layerName
    )

{-| Data type recieved from the server that is used to display a game.
-}

import Json.Decode as Decode exposing (Decoder, field)


type alias Image =
    { typ : String
    , value : ImageValue
    }


type alias ImageValue =
    { layers : List ImageLayer
    , width : Int
    , height : Int
    , name : String
    , value : Float
    }


type ImageLayer
    = ImageLayerHeatmap HeatmapValue
    | ImageLayerTokens TokensValue
    | ImageLayerActions ActionsValue
    | ImageLayerLines LinesValue


type alias HeatmapValue =
    { data : List Float
    , name : String
    , style : String
    , min : Float
    , max : Float
    }


type alias TokensValue =
    { data : List String
    , name : String
    , style : String
    }


type alias ActionsValue =
    { data : List Int
    , name : String
    }


type alias LinesValue =
    { data : List LineSegment
    , name : String
    }


type alias LineSegment =
    { begin : Float2D
    , end : Float2D
    }


type alias Float2D =
    { x : Float
    , y : Float
    }


layerName : ImageLayer -> String
layerName layer =
    case layer of
        ImageLayerHeatmap value ->
            value.name

        ImageLayerTokens value ->
            value.name

        ImageLayerActions value ->
            value.name

        ImageLayerLines value ->
            value.name


{-| Decodes a JSON object into an Image object.
-}
decode : Decoder Image
decode =
    Decode.map2 Image
        (field "typ" Decode.string)
        (field "value" decodeImageValue)


decodeImageValue : Decoder ImageValue
decodeImageValue =
    Decode.map5 ImageValue
        (field "layers" (Decode.list decodeLayerData))
        (field "width" Decode.int)
        (field "height" Decode.int)
        (field "name" Decode.string)
        (field "value" Decode.float)


decodeLayerData : Decoder ImageLayer
decodeLayerData =
    field "typ" Decode.string
        |> Decode.andThen decodeLayerDataHelp


decodeLayerDataHelp : String -> Decoder ImageLayer
decodeLayerDataHelp typ =
    case typ of
        "heatmap" ->
            decodeHeatmap

        "tokens" ->
            decodeTokens

        "actions" ->
            decodeActions

        "lines" ->
            decodeLines

        _ ->
            Decode.fail ("No Layer Data decoder implemented for " ++ typ)


decodeHeatmap : Decoder ImageLayer
decodeHeatmap =
    Decode.map ImageLayerHeatmap
        (field "value" decodeHeatmapValue)


decodeHeatmapValue : Decoder HeatmapValue
decodeHeatmapValue =
    Decode.map5 HeatmapValue
        (field "data" (Decode.list Decode.float))
        (field "name" Decode.string)
        (field "style" Decode.string)
        (field "min" Decode.float)
        (field "max" Decode.float)


decodeTokens : Decoder ImageLayer
decodeTokens =
    Decode.map ImageLayerTokens
        (field "value" decodeTokensValue)


decodeTokensValue : Decoder TokensValue
decodeTokensValue =
    Decode.map3 TokensValue
        (field "data" (Decode.list Decode.string))
        (field "name" Decode.string)
        (field "style" Decode.string)


decodeActions : Decoder ImageLayer
decodeActions =
    Decode.map ImageLayerActions
        (field "value" decodeActionsValue)


decodeActionsValue : Decoder ActionsValue
decodeActionsValue =
    Decode.map2 ActionsValue
        (field "data" (Decode.list Decode.int))
        (field "name" Decode.string)


decodeFloat2D : Decoder Float2D
decodeFloat2D =
    Decode.map2 Float2D
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)


decodeLineSegment : Decoder LineSegment
decodeLineSegment =
    Decode.map2 LineSegment
        (Decode.index 0 decodeFloat2D)
        (Decode.index 1 decodeFloat2D)


decodeLinesValue : Decoder LinesValue
decodeLinesValue =
    Decode.map2 LinesValue
        (field "data" (Decode.list decodeLineSegment))
        (field "name" Decode.string)


decodeLines : Decoder ImageLayer
decodeLines =
    Decode.map ImageLayerLines
        (field "value" decodeLinesValue)
