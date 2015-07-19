module View
    ( render
    ) where

import Color
import Dict
import Graphics.Element as Element exposing (Element)
import Graphics.Collage as Collage
import WarehouseKeeper
import Text

type alias RenderSetting =
    { unitSize : Int
    , windowWidth : Int
    , windowHeight : Int
    , origin : (Int, Int)
    }

render : RenderSetting -> WarehouseKeeper.Output -> Element
render rs {userState, warehouseMap, destinations, gameStatus} =
    let w = rs.windowWidth
        h = rs.windowHeight
        u = renderUser rs userState
        m = renderMap rs warehouseMap destinations
        explanation =
            Element.width w << Element.centered << Text.fromString <| "restart: space key"
        status =
            if gameStatus == WarehouseKeeper.Cleared
                then Element.width w << Element.centered << Text.fromString <| "game cleared"
                else Element.empty
        sourceLink =
            let url = "https://github.com/bigsleep/warehousekeeper"
                e = Element.width w << Element.centered << Text.fromString <| "source code"
            in Element.link url e
        v = Element.color Color.yellow <| Collage.collage w h (List.append u m)
    in Element.flow Element.down [v, explanation, status, sourceLink]

renderMap : RenderSetting -> WarehouseKeeper.WarehouseMap -> List (Int, Int) -> List Collage.Form
renderMap rs m ds =
    let unitSize = toFloat rs.unitSize
        wallForm = Collage.filled Color.black <| Collage.square unitSize
        objectForm = Collage.filled Color.blue <| Collage.square unitSize
        destForm = Collage.outlined (Collage.dashed Color.red) <| Collage.square unitSize
        renderTile (x, y) tile =
            move' rs (x, y) <|
                case tile of
                    WarehouseKeeper.TileWall -> wallForm
                    WarehouseKeeper.TileBox _ -> objectForm
                    WarehouseKeeper.TileEmpty -> Collage.toForm Element.empty
        tiles = Dict.values <| Dict.map renderTile <| m
        renderDest (x, y) =
            move' rs (x, y) destForm
        dests = List.map renderDest ds
    in List.append tiles dests

renderUser : RenderSetting -> WarehouseKeeper.UserState -> List Collage.Form
renderUser rs us =
    let (x, y) = us.position
        unitSize = toFloat rs.unitSize
        form = Collage.filled Color.red <| Collage.square unitSize
    in [move' rs (x, y) form]

move' rs (x, y) =
    let u = rs.unitSize
        (ox, oy) = rs.origin
    in Collage.move (toFloat (ox + x * u), toFloat (oy - y * u))
