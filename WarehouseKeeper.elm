module WarehouseKeeper
    ( Tile(..)
    , GameStatus(..)
    , WarehouseMap
    , UserState
    , Action(..)
    , UserAction(..)
    , ObjectAction(..)
    , ActionResult(..)
    , Output
    , makeGame
    ) where

import Array exposing (Array)
import Automaton as Auto exposing ((>>>), (<<<))
import Color
import Dict exposing (Dict)
import Graphics.Element as Element exposing (Element)
import Graphics.Collage as Collage
import Signal as Sig exposing (Signal, (<~))
import String

type alias Auto a b = Auto.Automaton a b

type Tile =
    TileEmpty |
    TileWall |
    TileBox Int

type GameStatus =
    Playing |
    Cleared

type alias WarehouseMap = Dict (Int, Int) Tile

type alias UserState =
    { position : (Int, Int)
    }

type Action =
    ActionOnUser UserAction |
    ActionOnObject Int ObjectAction

type UserAction =
    MoveUser (Int, Int)

type ObjectAction =
    MoveObject (Int, Int)

type ActionResult =
    Nochange |
    Success |
    NextAction Action

type alias Output =
    { userState : UserState
    , warehouseMap : WarehouseMap
    , destinations : List (Int, Int)
    , gameStatus : GameStatus
    }

user : UserState -> Auto (UserAction, Output) UserState
user init =
    let f (a, s) u = case a of
        MoveUser (dx, dy) ->
            let p = u.position
                (x, y) = p
                p' = (x + dx, y + dy)
                next = case Dict.get p' s.warehouseMap of
                    Just TileEmpty -> p'
                    otherwise -> p
            in { u | position <- next }
        otherwise -> u
    in Auto.state init f

box : (Int, Int) -> Auto (ObjectAction, Output) (ActionResult, (Int, Int))
box p =
    let f (a, s) q =
        case a of
            MoveObject (dx, dy) ->
                let (x, y) = q
                    q' = (x + dx, y + dy)
                    (r, next) = case Dict.get q' s.warehouseMap of
                        Just TileEmpty -> (NextAction (ActionOnUser (MoveUser (dx, dy))), q')
                        otherwise -> (Nochange, q)
                in ((r, next), next)
            otherwise -> ((Nochange, q), q)
    in Auto.hiddenState p f

warehouseKeeper :
    WarehouseMap ->
    List (Int, Int) ->
    Output ->
    Auto ((Int, Int), Bool) Output
warehouseKeeper wall b0 s0 =
    let u0 = s0.userState
        m0 = s0.warehouseMap
        dests = s0.destinations

        u : Auto (UserAction, Output) UserState
        u = user s0.userState

        u' : Auto (Action, Output) Output
        u' =
            Auto.branch (Auto.pure snd) (filterMap onUserAction u0 u) >>>
            Auto.pure (\(s, us) -> { s | userState <- us })

        boxes = List.map2 (,) [0..(List.length b0 - 1)] b0

        boxes' : List (Auto (Action, Output) (Int, (Maybe ActionResult, (Int, Int))))
        boxes' = List.map makeBox boxes

        boxes'' : Auto (Action, Output) (ActionResult, Output)
        boxes'' =
            Auto.branch (Auto.pure snd) (Auto.combine boxes') >>>
            Auto.pure toOutput

        game : Auto (Action, Output) Output
        game =
            Auto.branch (Auto.pure fst) boxes'' >>>
            Auto.pure mergeActionResult >>>
            u'

        game' : Auto ((Int, Int), Output) Output
        game' =
            filter ((/=) Cleared << (.gameStatus) << snd) s0 <|
            Auto.branch (Auto.pure toAction) (Auto.pure snd) >>> game

        game'' : Auto (Int, Int) Output
        game'' = Auto.loop s0 (game' >>> Auto.pure (\x -> (x, x)))

        game''' : Auto ((Int, Int), Bool) Output
        game''' =
            let sw = Auto.pure (\(a, b) -> (a, if b then Just () else Nothing)) >>> Auto.first game''
            in switch sw (\_ -> game''')

        onUserAction (a, s) = case a of
            ActionOnUser ua -> Just (ua, s)
            otherwise -> Nothing

        onObjectAction i (a, s) = case a of
            ActionOnObject j oa -> if i == j then Just (oa, s) else Nothing
            otherwise -> Nothing

        toAction : ((Int, Int), Output) -> Action
        toAction ((dx, dy), s) =
            let (x, y) = s.userState.position
                target = (x + dx, y + dy)
                tile = Dict.get target s.warehouseMap
            in case tile of
                    Just (TileBox i) -> ActionOnObject i (MoveObject (dx, dy))
                    otherwise -> ActionOnUser (MoveUser (dx, dy))

        makeBox (i, b') =
            Auto.pure (onObjectAction i) >>>
            perJust (box b') >>>
            Auto.pure (\x -> (Maybe.map fst x, Maybe.map snd x)) >>>
            Auto.second (hold b') >>>
            Auto.pure ((,) i)

        mergeActionResult (a, (r, s)) =
            case r of
                NextAction na -> (na, s)
                otherwise -> (a, s)

        toOutput (s, xs) =
            let b = List.map (snd << snd) xs
                m = makeWarehouseMap wall << List.map (\(i, (_, p)) -> (i, p)) <| xs
                r = Maybe.withDefault Nochange << msum << List.map (fst << snd) <| xs
                g = if List.sort b == s.destinations then Cleared else Playing
            in (r, { s | warehouseMap <- m, gameStatus <- g })

        msum xs = case xs of
            Just x :: _ -> Just x
            Nothing :: xs' -> msum xs'
            otherwise -> Nothing

    in game'''

switch : Auto a (b, Maybe c) -> (c -> Auto a b) -> Auto a b
switch sw0 k0 =
    let f a (sw1, k1) =
        case Auto.step a sw1 of
            (_, (b, Just c)) ->
                let sw2 = k1 c >>> Auto.pure (\x -> (x, Nothing)) 
                in (b, (sw2, k1))
            (sw2, (b, Nothing)) ->
                (b, (sw2, k1))
    in Auto.hiddenState (sw0, k0) f

hold : a -> Auto (Maybe a) a
hold default =
    let f a s = case a of
        Just a' -> (a', a')
        Nothing -> (s, s)
    in Auto.hiddenState default f

perJust : Auto a b -> Auto (Maybe a) (Maybe b)
perJust auto =
    let f a s = case a of
        Just x ->
            let (s', y) = Auto.step x s
            in (Just y, s')
        Nothing ->
            (Nothing, s)
    in Auto.hiddenState auto f

filter : (a -> Bool) -> b -> Auto a b -> Auto a b
filter f default =
    let g a = if f a then Just a else Nothing
    in filterMap g default

filterMap : (a -> Maybe b) -> c ->  Auto b c -> Auto a c
filterMap f default auto =
    Auto.pure f >>> perJust auto >>> hold default

makeWarehouseMap : WarehouseMap -> List (Int, (Int, Int)) -> WarehouseMap
makeWarehouseMap wall outputs =
    let tiles = List.map f <| outputs
        f (i, x) = (x, TileBox i)
        m = Dict.fromList tiles
    in Dict.union m wall

makeGame : String -> (Auto ((Int, Int), Bool) Output, Output)
makeGame input =
    let layout = String.lines input

        zipIndexRows rows = List.map2 (,) [0..(List.length rows - 1)] rows

        zipIndexCol (j, row) = List.map (\(i, a) -> ((i, j), a)) << List.map2 (,) [0..(String.length row - 1)] << String.toList <| row

        init (pos, letter) (xs, bs, dests, start) =
            case letter of
                'W' -> ((pos, TileWall) :: xs, bs, dests, start)
                'S' -> ((pos, TileEmpty) :: xs, bs, dests, pos)
                'B' -> ((pos, TileEmpty) :: xs, pos :: bs, dests, start)
                'D' -> ((pos, TileEmpty) :: xs, bs, pos :: dests, start)
                otherwise ->
                    ((pos, TileEmpty) :: xs, bs, dests, start)

        xs = List.concat << List.map zipIndexCol << zipIndexRows << List.filter ((/=) "") <| layout

        (tiles, boxes, dests, start) = List.foldr init ([], [], [], (1, 1)) xs

        u = { position = start }

        wall = Dict.fromList tiles

        m = makeWarehouseMap wall (List.map2 (,) [0..(List.length boxes - 1)] boxes)

        s = { userState = u, warehouseMap = m, destinations = List.sort dests, gameStatus = Playing }

    in (warehouseKeeper wall boxes s, s)
