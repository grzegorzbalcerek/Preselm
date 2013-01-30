-- Copyright (c) 2013 Grzegorz Balcerek; see the LICENSE.txt file

module Preselm (presentation,emptyFrame) where

import Maybe (justs, maybe)

-- util

ithmod n (h:t) =
  let nmod = n `mod` (length (h:t))
  in if nmod == 0 then h else (ithmod (nmod-1) t)

isEmpty lst = case lst of { [] -> True; _ -> False }

maybe_map f m = case m of { Just x -> Just (f x); Nothing -> Nothing }

-- signals

data Transition = NoTransition | ForwardTransition | BackwardTransition

lastKeysDownSignal = sampleOn (dropIf isEmpty [] Keyboard.Raw.keysDown) Keyboard.Raw.keysDown

currentFrameIndexSignal =
  let step (t,keys) indexes =
    let (nextIndex,trans) = 
        if | keys == [39] || keys == [78] || keys == [13] -> (indexes.current+1,ForwardTransition)
           | keys == [37] || keys == [80] -> (indexes.current-1,BackwardTransition)
           | keys == [35] || keys == [69] -> (0-1,NoTransition)
           | keys == [36] || keys == [72] -> (0,NoTransition)
           | otherwise -> (indexes.current,NoTransition)
    in { current = nextIndex, previous = indexes.current, indexChangeTime = t, transition = trans }
  in foldp step { current=0, previous=0, indexChangeTime=0, transition = NoTransition } (timestamp lastKeysDownSignal)

-- this signal has the elapsed time since the last frame index change, updated with the frequency of fps 60, 
timeSinceIndexChangeSignal = timeOf (60 `fpsWhen` (second `since` currentFrameIndexSignal))

-- context

makeContextRecord w h i tsic md mpd mp =
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

contextSignal = makeContextRecord <~ Window.width
                                   ~ Window.height
                                   ~ currentFrameIndexSignal
                                   ~ timeSinceIndexChangeSignal
                                   ~ Mouse.isDown
                                   ~ sampleOn Mouse.isDown Mouse.position
                                   ~ Mouse.position

-------------------------------------- FRAME BUILDERS --------------------


contextDebugBuilder =
  let contextDebugElement frame context = Just $ container context.windowWidth context.windowHeight midBottom (asText context)
  in [ contextDebugElement ]


backgroundBuilders =
  let backgroundColorElement frame context =
    let f x = collage context.windowWidth context.windowHeight [
                filled x $ rect context.windowWidth context.windowHeight
                             ((toFloat context.windowWidth)/2.0,(toFloat context.windowHeight)/2.0) ]
    in maybe_map f frame.backgroundColor
  in [ backgroundColorElement ]

headerBuilders =
  let titleElement frame context =
        let headerHeight = context.windowHeight * (maybe 0.0 id frame.headerHeight)
            f x = container context.windowWidth headerHeight middle (text . header . toText $ x)
        in maybe_map f frame.title
      headerBackgroundElement frame context =
        let headerHeight = context.windowHeight * (maybe 0.0 id frame.headerHeight)
            f x = collage context.windowWidth headerHeight [
                     filled x $ rect context.windowWidth headerHeight
                             ((toFloat context.windowWidth)/2.0,(headerHeight)/2.0) ]
        in maybe_map f frame.headerBackgroundColor
  in  [ headerBackgroundElement, titleElement ]


contentBuilders =
  let contentElement frame context =
    let floatWidth = toFloat context.windowWidth
        leftMargin = maybe 0.0 id frame.leftMargin
        topMargin = maybe 0.0 id frame.topMargin
        contentWidth = maybe 1.0 id frame.contentWidth
        f x = container context.windowWidth context.windowHeight
                 (topLeftAt (relative leftMargin)(relative topMargin))
                 (width (truncate (floatWidth * contentWidth)) x)
    in maybe_map f frame.content
  in  [ contentElement ]

middleBuilders =
  let middleElement frame context =
    let f x = container context.windowWidth context.windowHeight middle x
    in maybe_map f frame.middle
  in  [ middleElement ]


twoColumnsBuilders =
  let column1Element frame context =
        let leftMargin = maybe 0.0 id frame.leftMargin
            topMargin = maybe 0.0 id frame.topMargin
            floatWidth = toFloat context.windowWidth
            columnWidth = maybe 0.5 id frame.columnWidth
            f x = container context.windowWidth context.windowHeight
                                             (topLeftAt (relative leftMargin)(relative topMargin))
                                             (width (truncate (floatWidth * columnWidth)) x)
        in maybe_map f frame.column1
      column2Element frame context =
        let rightMargin = maybe 0.0 id frame.rightMargin
            topMargin = maybe 0.0 id frame.topMargin
            floatWidth = toFloat context.windowWidth
            columnWidth = maybe 0.5 id frame.columnWidth
            f x = container context.windowWidth context.windowHeight
                                             (topRightAt (relative rightMargin)(relative topMargin))
                                             (width (truncate (floatWidth * columnWidth)) x)
        in maybe_map f frame.column2
  in  [ column1Element, column2Element ]

selectionBoxBuilder =
  let selectionBoxElement frame context =
    if Maybe.isJust frame.selectionBoxColor
    then
      if context.mouseIsDown
      then let (x0,y0) = context.mousePosition
               (x1,y1) = context.mousePositionOnDown
               (xL,xR) = if x0 < x1 then (x0,x1) else (x1,x0)
               (yU,yD) = if y0 < y1 then (y0,y1) else (y1,y0)
               w = xR - xL
               h = yD - yU
               center = ((xR+xL)/2, (yD+yU)/2)
               color = maybe white id frame.selectionBoxColor
           in Just $ collage context.windowWidth context.windowHeight [
                       outlined color $ rect (w-4) (h-4) center ,
                       outlined color $ rect (w-2) (h-2) center ,
                       outlined color $ rect w h center ]
      else Nothing
    else Nothing
  in [ selectionBoxElement ]

coreFrameBuilders = concat [ backgroundBuilders, headerBuilders, contentBuilders, middleBuilders, twoColumnsBuilders, selectionBoxBuilder]
frameBuilders = coreFrameBuilders -- ++ [contextDebugBuilder]
buildFrame frame context = layers (justs $ map (\f -> f frame context) frameBuilders)

----------------------------- HANDLERS

slidingTransitionSelectors = 
  let twoFramesElement leftFrame rightFrame context = (buildFrame leftFrame context) `beside` (buildFrame rightFrame context)
      getDelta windowWidth tsic = toFloat windowWidth * (1 - 0.9^(tsic `div` 10))
      moveFramesLeftToRight frames context =
          let leftFrame = ithmod context.previousFrameIndex frames
              rightFrame = ithmod context.currentFrameIndex frames
              deltaX = getDelta context.windowWidth context.timeSinceIndexChange
              twoFrames = twoFramesElement leftFrame rightFrame context
              position = topLeftAt (absolute (0-deltaX)) (absolute 0)
          in container context.windowWidth context.windowHeight position twoFrames
      moveFramesRightToLeft frames context =
          let leftFrame = ithmod context.currentFrameIndex frames
              rightFrame = ithmod context.previousFrameIndex frames
              deltaX = getDelta context.windowWidth context.timeSinceIndexChange
              twoFrames = twoFramesElement leftFrame rightFrame context
              position = topLeftAt (absolute (deltaX-context.windowWidth)) (absolute 0)
          in container context.windowWidth context.windowHeight position twoFrames
      selectLTR context =
        if context.transition == ForwardTransition && context.timeSinceIndexChange < 1000
        then Just moveFramesLeftToRight
        else Nothing
      selectRTL context =
        if context.transition == BackwardTransition && context.timeSinceIndexChange < 1000
        then Just moveFramesRightToLeft
        else Nothing
  in  [ selectLTR, selectRTL ]

defaultHandlerSelector =
  let showCurrentFrame frames context = buildFrame (ithmod context.currentFrameIndex frames) context
      selectShowFrame context = Just showCurrentFrame
  in [ selectShowFrame ] 

handlerSelectors = concat [slidingTransitionSelectors, defaultHandlerSelector]
      
-- presentation

emptyFrame = {
 backgroundColor = Nothing,
 column1 = Nothing,
 column2 = Nothing,
 columnWidth = Just 0.35,
 content = Nothing,
 contentWidth = Just 0.8,
 headerBackgroundColor = Nothing,
 headerHeight = Just 0.1,
 leftMargin = Just 0.1,
 middle = Nothing,
 rightMargin = Just 0.1,
 selectionBoxColor = Nothing,
 title = Nothing,
 topMargin = Just 0.15}

presentation frames =
  let showPresentation frames context = 
        let handlers = justs $ map (\f -> f context) handlerSelectors
            theHandler = head handlers
        in theHandler frames context
  in lift (showPresentation frames) contextSignal
