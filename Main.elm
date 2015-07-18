module Main where

import Automaton as Auto
import Char
import Dict
import Graphics.Element as Element exposing (Element)
import Keyboard
import WarehouseKeeper
import Signal exposing ((<~))
import View

m1 = """
WWWWWWWWWW
WS       W
W      B W
W      B W
WDDD   B W
WWWWWWWWWW
"""

main =
    let (game, o) = WarehouseKeeper.makeGame m1
        input = Signal.map2 (\{x, y} restart -> ((x, -y), restart)) Keyboard.arrows Keyboard.space
        (mw, mh) = (\(w, h) -> (w + 1, h + 1)) << Maybe.withDefault (0, 0) << List.maximum << Dict.keys <| o.warehouseMap
        u = 40
        w = mw * u
        h = mh * u
        ox = (-w + u) // 2
        oy = (h - u) // 2
        renderSetting = {unitSize = u, windowWidth = w, windowHeight = h, origin = (ox, oy)}
    in View.render renderSetting <~ Auto.run game o input
