module Image exposing
    ( ActionsValue
    , HeatmapValue
    , Image
    , ImageLayer(..)
    , ImageValue
    , TokensValue
    , decode
    )

{-| Data type recieved from the server that is used to display a game.
-}

import Json.Decode as Decode


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


{-| Decodes a JSON object into an Image object.
-}
decode : Decode.Decoder Image
decode =
    Decode.map2 Image
        (Decode.field "typ" Decode.string)
        (Decode.field "value" decodeImageValue)


decodeImageValue : Decode.Decoder ImageValue
decodeImageValue =
    Decode.map5 ImageValue
        (Decode.field "layers" (Decode.list decodeLayerData))
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "value" Decode.float)


decodeLayerData : Decode.Decoder ImageLayer
decodeLayerData =
    Decode.field "typ" Decode.string
        |> Decode.andThen decodeLayerDataHelp


decodeLayerDataHelp : String -> Decode.Decoder ImageLayer
decodeLayerDataHelp typ =
    case typ of
        "heatmap" ->
            decodeHeatmap

        "tokens" ->
            decodeTokens

        "actions" ->
            decodeActions

        _ ->
            Decode.fail ("No Layer Data decoder implemented for " ++ typ)


decodeHeatmap : Decode.Decoder ImageLayer
decodeHeatmap =
    Decode.map ImageLayerHeatmap
        (Decode.field "value" decodeHeatmapValue)


decodeHeatmapValue : Decode.Decoder HeatmapValue
decodeHeatmapValue =
    Decode.map5 HeatmapValue
        (Decode.field "data" (Decode.list Decode.float))
        (Decode.field "name" Decode.string)
        (Decode.field "style" Decode.string)
        (Decode.field "min" Decode.float)
        (Decode.field "max" Decode.float)


decodeTokens : Decode.Decoder ImageLayer
decodeTokens =
    Decode.map ImageLayerTokens
        (Decode.field "value" decodeTokensValue)


decodeTokensValue : Decode.Decoder TokensValue
decodeTokensValue =
    Decode.map3 TokensValue
        (Decode.field "data" (Decode.list Decode.string))
        (Decode.field "name" Decode.string)
        (Decode.field "style" Decode.string)


decodeActions : Decode.Decoder ImageLayer
decodeActions =
    Decode.map ImageLayerActions
        (Decode.field "value" decodeActionsValue)


decodeActionsValue : Decode.Decoder ActionsValue
decodeActionsValue =
    Decode.map2 ActionsValue
        (Decode.field "data" (Decode.list Decode.int))
        (Decode.field "name" Decode.string)
