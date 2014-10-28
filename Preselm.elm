-- Copyright (c) 2013 Grzegorz Balcerek; see the LICENSE.txt file

module Preselm (presentation, emptyFrame, Forward, Backward, Home, End) where

import Maybe (..)
import Mouse
import Text

-- util

ithmod : Int -> [a] -> a
ithmod n (h::t) =
    let nmod = n `mod` length (h::t)
    in  if nmod == 0
            then h
            else ithmod (nmod - 1) t

isEmpty : [a] -> Bool
isEmpty lst = case lst of
                [] -> True
                _  -> False

mapPair : (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

-- signals

data Transition = NoTransition | ForwardTransition | BackwardTransition
type FrameIndex =
    { current : Int
    , previous : Int
    , indexChangeTime : Time
    , transition : Transition
    }

data Action = Forward | Backward | Home | End

currentFrameIndexSignal : Signal a -> (a -> Maybe Action) -> Signal FrameIndex
currentFrameIndexSignal keysDown handle =
    let step (t, keys) indexes =
        let (nextIndex, trans) =
            case handle keys of
                Just Forward  -> (indexes.current + 1, ForwardTransition)
                Just Backward -> (indexes.current - 1, BackwardTransition)
                Just Home     -> (0, NoTransition)
                Just End      -> (0 - 1, NoTransition)
                Nothing       -> (indexes.current, NoTransition)
        in { current = nextIndex, previous = indexes.current, indexChangeTime = t, transition = trans }
    in foldp step { current = 0, previous = 0, indexChangeTime = 0, transition = NoTransition } (timestamp keysDown)

-- this signal has the elapsed time since the last frame index change, updated with the frequency of fps 60
timeSinceIndexChangeSignal : Signal FrameIndex -> Signal Time
timeSinceIndexChangeSignal frames = fst <~ (timestamp <| fpsWhen 60 (since second frames))

-- context

type Context =
    { windowWidth : Int
    , windowHeight : Int
    , currentFrameIndex : Int
    , previousFrameIndex : Int
    , timeSinceIndexChange : Time
    , transition : Transition
    , mouseIsDown : Bool
    , mousePositionOnDown : (Int, Int)
    , mousePosition : (Int, Int)
    }

makeContextRecord : (Int, Int) -> FrameIndex -> Time -> Bool -> (Int, Int) -> (Int, Int) -> Context
makeContextRecord (w, h) i tsic md mpd mp =
    { windowWidth = w
    , windowHeight = h
    , currentFrameIndex = i.current
    , previousFrameIndex = i.previous
    , timeSinceIndexChange = tsic-i.indexChangeTime
    , transition = i.transition
    , mouseIsDown = md
    , mousePositionOnDown = mpd
    , mousePosition = mp
    }

makeContextSignal : Signal (Int, Int) -> Signal FrameIndex -> Signal Context
makeContextSignal dimensions frames =
    makeContextRecord <~ dimensions
                       ~ frames
                       ~ (timeSinceIndexChangeSignal frames)
                       ~ Mouse.isDown
                       ~ (sampleOn Mouse.isDown Mouse.position)
                       ~ Mouse.position

------------------------------------ FRAME BUILDERS --------------------

type MaybeFrameBuilder = [Frame -> Context -> Maybe Element]
type FrameBuilder = Frame -> Context -> Element

contextDebugBuilder : MaybeFrameBuilder
contextDebugBuilder =
    let contextDebugElement frame context = Just <| container context.windowWidth context.windowHeight midBottom (asText context)
    in [ contextDebugElement ]


backgroundBuilders : MaybeFrameBuilder
backgroundBuilders =
    let backgroundColorElement frame context =
        case frame.backgroundColor of
            Just c -> Just <| collage context.windowWidth context.windowHeight
                                [ filled c <| rect (toFloat context.windowWidth) (toFloat context.windowHeight) ]
            Nothing -> Nothing
    in [ backgroundColorElement ]

headerBuilders : MaybeFrameBuilder
headerBuilders =
    let titleElement frame context =
            case frame.title of
                Just s  -> let headerHeight = (toFloat context.windowHeight) * (maybe 0 id frame.headerHeight)
                           in  Just <| container context.windowWidth (round headerHeight) middle (centered . Text.height 40 . bold . toText <| s)
                Nothing -> Nothing
        headerBackgroundElement frame context =
            case frame.headerBackgroundColor of
                Just c  -> let headerHeight = (toFloat context.windowHeight) * (maybe 0 id frame.headerHeight)
                           in  Just <| collage context.windowWidth (round headerHeight)
                                            [ filled c <| rect (toFloat context.windowWidth) headerHeight ]
                Nothing -> Nothing
    in  [ headerBackgroundElement, titleElement ]

contentBuilders : MaybeFrameBuilder
contentBuilders =
    let contentElement frame context =
        case frame.content of
            Just elem -> let floatWidth   = toFloat context.windowWidth
                             leftMargin   = maybe 0 id frame.leftMargin
                             topMargin    = maybe 0 id frame.topMargin
                             contentWidth = maybe 1 id frame.contentWidth
                         in  Just <| container context.windowWidth context.windowHeight
                                        (topLeftAt (relative leftMargin) (relative topMargin))
                                        (width (truncate (floatWidth * contentWidth)) elem)
            Nothing   -> Nothing
    in  [ contentElement ]

middleBuilders : MaybeFrameBuilder
middleBuilders =
    let middleElement frame context =
        case frame.middle of
            Just elem -> Just <| container context.windowWidth context.windowHeight middle elem
            Nothing   -> Nothing
    in  [ middleElement ]

twoColumnsBuilders : MaybeFrameBuilder
twoColumnsBuilders =
    let column1Element frame context =
            case frame.column1 of
                Just elem -> let leftMargin  = maybe 0 id frame.leftMargin
                                 topMargin   = maybe 0 id frame.topMargin
                                 floatWidth  = toFloat context.windowWidth
                                 columnWidth = maybe 0 id frame.columnWidth
                             in  Just <| container context.windowWidth context.windowHeight
                                            (topLeftAt (relative leftMargin) (relative topMargin))
                                            (width (truncate (floatWidth * columnWidth)) elem)
                Nothing   -> Nothing
        column2Element frame context =
            case frame.column2 of
                Just elem -> let rightMargin = maybe 0 id frame.rightMargin
                                 topMargin   = maybe 0 id frame.topMargin
                                 floatWidth  = toFloat context.windowWidth
                                 columnWidth = maybe 0 id frame.columnWidth
                             in  Just <| container context.windowWidth context.windowHeight
                                            (topRightAt (relative rightMargin) (relative topMargin))
                                            (width (truncate (floatWidth * columnWidth)) elem)
                Nothing   -> Nothing
    in  [ column1Element, column2Element ]

selectionBoxBuilder : MaybeFrameBuilder
selectionBoxBuilder =
    let selectionBoxElement frame context =
        case frame.selectionBoxColor of
            Just c  -> if | context.mouseIsDown ->
                                let (x0,y0) = mapPair toFloat context.mousePosition
                                    (x1,y1) = mapPair toFloat context.mousePositionOnDown
                                    (xL,xR) = if x0 < x1 then (x0, x1) else (x1, x0)
                                    (yU,yD) = if y0 < y1 then (y0, y1) else (y1, y0)
                                    w       =  xR - xL
                                    h       =  yD - yU
                                    x       = (x0 + x1) / 2 - (toFloat context.windowWidth) / 2
                                    y       = (toFloat context.windowHeight) / 2 - (y1 + y0) / 2
                                in Just <| collage context.windowWidth context.windowHeight
                                                [ move (x, y) . outlined (solid c) <| rect (w - 4) (h - 4)
                                                , move (x, y) . outlined (solid c) <| rect (w - 2) (h - 2)
                                                , move (x, y) . outlined (solid c) <| rect  w       h
                                                ]

                          | otherwise -> Nothing
            Nothing -> Nothing
    in [ selectionBoxElement ]

coreFrameBuilders : MaybeFrameBuilder
coreFrameBuilders = concat [ backgroundBuilders, headerBuilders, contentBuilders, middleBuilders, twoColumnsBuilders, selectionBoxBuilder]

frameBuilders : MaybeFrameBuilder
frameBuilders = coreFrameBuilders -- ++ [contextDebugBuilder]

buildFrame : FrameBuilder
buildFrame frame context = layers (justs <| map (\f -> f frame context) frameBuilders)

----------------------------- HANDLERS

type HandlerSelector = Context -> Maybe ([Frame] -> Context -> Element)

slidingTransitionSelectors : [HandlerSelector]
slidingTransitionSelectors =
    let twoFramesElement leftFrame rightFrame context = (buildFrame leftFrame context) `beside` (buildFrame rightFrame context)
        getDelta windowWidth tsic = (toFloat windowWidth) * (1 - 0.9^(tsic / 10))
        moveFramesLeftToRight frames context =
            let leftFrame  = ithmod context.previousFrameIndex frames
                rightFrame = ithmod context.currentFrameIndex frames
                deltaX     = getDelta context.windowWidth context.timeSinceIndexChange
                twoFrames  = twoFramesElement leftFrame rightFrame context
                position   = topLeftAt (absolute (round -deltaX)) (absolute 0)
            in  container context.windowWidth context.windowHeight position twoFrames
        moveFramesRightToLeft frames context =
            let leftFrame  = ithmod context.currentFrameIndex frames
                rightFrame = ithmod context.previousFrameIndex frames
                deltaX     = getDelta context.windowWidth context.timeSinceIndexChange
                twoFrames  = twoFramesElement leftFrame rightFrame context
                position   = topLeftAt (absolute (round <| deltaX - (toFloat context.windowWidth))) (absolute 0)
            in  container context.windowWidth context.windowHeight position twoFrames
        selectLTR context =
            if context.transition == ForwardTransition && context.timeSinceIndexChange < 1000
                then Just moveFramesLeftToRight
                else Nothing
        selectRTL context =
            if context.transition == BackwardTransition && context.timeSinceIndexChange < 1000
                then Just moveFramesRightToLeft
                else Nothing
    in  [ selectLTR, selectRTL ]

defaultHandlerSelector : [HandlerSelector]
defaultHandlerSelector =
    let showCurrentFrame frames context = buildFrame (ithmod context.currentFrameIndex frames) context
        selectShowFrame context = Just showCurrentFrame
    in [ selectShowFrame ]

handlerSelectors : [HandlerSelector]
handlerSelectors = concat [slidingTransitionSelectors, defaultHandlerSelector]

-- presentation

type Frame = { backgroundColor : Maybe Color
             , column1 : Maybe Element
             , column2 : Maybe Element
             , columnWidth : Maybe Float
             , content : Maybe Element
             , contentWidth : Maybe Float
             , headerBackgroundColor : Maybe Color
             , headerHeight : Maybe Float
             , leftMargin : Maybe Float
             , middle : Maybe Element
             , rightMargin : Maybe Float
             , selectionBoxColor : Maybe Color
             , title : Maybe String
             , topMargin : Maybe Float
             }

emptyFrame : Frame
emptyFrame = { backgroundColor = Nothing
             , column1 = Nothing
             , column2 = Nothing
             , columnWidth = Just 0.35
             , content = Nothing
             , contentWidth = Just 0.8
             , headerBackgroundColor = Nothing
             , headerHeight = Just 0.1
             , leftMargin = Just 0.1
             , middle = Nothing
             , rightMargin = Just 0.1
             , selectionBoxColor = Nothing
             , title = Nothing
             , topMargin = Just 0.15
             }

presentation : [Frame] -> Signal (Int, Int) -> Signal a -> (a -> Maybe Action) -> Signal Element
presentation frames dimensions keys handle =
  let currentFrames = currentFrameIndexSignal keys handle
      context = makeContextSignal dimensions currentFrames
      showPresentation frames context =
        let handlers = justs <| map (\f -> f context) handlerSelectors
            theHandler = head handlers
        in  theHandler frames context
  in lift (showPresentation frames) context
