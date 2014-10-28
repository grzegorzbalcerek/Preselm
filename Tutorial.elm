-- Copyright (c) 2013 Grzegorz Balcerek; see the LICENSE.txt file

module Tutorial where

import Window
import Keyboard

import Preselm (..)
import Preselm

--------------------------

frame1 = { emptyFrame | middle <- Just [markdown|# Preselm Tutorial
Press *Enter* to continue.|] }

--------------------------

frame2 = { emptyFrame | title <- Just "Navigation", middle <- Just [markdown|
# Welcome to the Preselm Tutorial!

The goal of Preselm is to make it possible and easy to create presentations using the Elm programming language.

Preselm is written in Elm. If you do not know Elm, you can take a look at its [website](http://elm-lang.org/).

Before we continue, let’s see how you can navigate through this presentation.

### Here is what you can do to move forward or backward:

- press the _**right arrow**_, _**Enter**_ or _**n**_ to go to the next frame
- press the _**left arrow**_ or _**p**_ to go to the previous frame
- press _**home**_ or _**h**_ button to go to the first frame
- press _**end**_ or _**e**_ to go to the last frame

### Now, press *Enter* to go to the next frame|] }

--------------------------

hwProgram = "import Preselm

frame1 = { emptyFrame | middle <- Just [markdown|# Hello World!|] }

frame2 = { emptyFrame | title <- Just \"Goodbye\", middle <- Just [markdown|#World!|] }

main = presentation [frame1, frame2]"

frame3 = { emptyFrame | title <- Just "Hello World", column1 <- Just <| [markdown|Let’s create a simple presentation. You can follow the steps described here.

Create a centered file called *HelloWorld.elm* with the content shown on the right hand side.

Then you need to compile it together with Preselm. Here is how you can do it:

    elm --make HelloWorld.elm

If the compilation is successful you should get the *HelloWorld.html* file with your presentation.

When you open it in a browser, you should get a Preselm presentation with two frames, which look like the next two frames of this tutorial.

After those two frames the tutorial will continue.

Press *Enter* to continue. |],
 column2 <- Just (centered <| monospace <| toText hwProgram) }

--------------------------

hwframe1 = { emptyFrame | middle <- Just [markdown|# Hello World!|] }

--------------------------

hwframe2 = { emptyFrame | title <- Just "Goodbye", middle <- Just [markdown|#World!|] }

--------------------------

frame4 = { emptyFrame | column1 <- Just <| [markdown|

We are back to the tutorial.

Let’s analyze the program line by line. Actually, it is a valid Elm program, so when you write your presentation, you should follow the Elm syntax.

The first line imports Preselm.

The second and third lines define both frames of the presentation.

The last frame defines what the Elm program should actually do by defining the `main` function. Since we create a Preselm presentation, we define `main` to call the `presentation` function (imported from Preselm) and we give it as its argument the list of frames to be included in the presentation. The frame names (here: `frame1` and `frame2`) are not important, they should just be consistent with the names defined earlier.|],
 column2 <- Just [markdown|Let’s go back to the second line. It defines the first frame. The frame definition is a record with fields describing the frame. Since there are many possible fields, Preselm provides a default record, called `emptyFrame`, which includes all the fields with default values. When defining our frames we use that default frame and only update relevant fields that we want to change. We use the Elm record update syntax to do that.

Each record field has an optional value, which means it is a value wrapped in a Maybe value.

Our first frame updates only one field, called `middle`. That field has the type of `Maybe Element` and its purpose is to show the element in the middle of the frame.

The second frame updates two fields: `title` and `middle`. The `title` field has the type of `Maybe String` and its purpose is to show the frame title on the top section of the frame.|] }

--------------------------

frame5 = { emptyFrame | title <- Just "Preselm fields", content <- Just [markdown|
The following is the list of fields that you can use in the frames of a Preselm presentation.
Each item on the list contains the field name, type and a short description.

- `backgroundColor :: Maybe Color` — the frame background color
- `column1 :: Maybe String` — an element shown on the left side of the frame, placed `leftMargin` from the left, its width is specified by the value of the `columnWidth` field
- `column2 :: Maybe String` — an element shown on the right side of the frame, placed `rightMargin` from the right, its width is specified by the value of the `columnWidth` field
- `columnWidth :: Maybe Float` — the width of a column specified as a fraction of the frame width; its default value is `Just 0.35`
- `content :: Maybe Element` — the frame content, placed `leftMargin` from the left and `topMargin` from the top of the frame; its width is specified by `contentWidth`
- `contentWidth :: Maybe Float` — the width of the frame content specified as a fraction of the frame width; its default value is `Just 0.8`
- `headerBackgroundColor :: Maybe Color` — the header background color
- `headerHeight :: Maybe Float` — the header height specified as a fraction of the frame height; its default value is `Just 0.1`
- `leftMargin :: Maybe Float` — the left margin specified as a fraction of the frame width; its default value is `Just 0.1`
- `middle :: Maybe Element` — an element shown in the middle of the frame
- `rightMargin :: Maybe Float` — the right margin specified as a fraction of the frame width; its default value is `Just 0.1`
- `selectionBoxColor :: Maybe Color` — the color of the selection box
- `title :: Maybe String` — the frame title, shown in the frame header
- `topMargin :: Maybe Float` — the top margin specified as a fraction of the frame height; its default value is `Just 0.15`

Let’s now see some of them in action. Press *Enter* to continue.
|] }

--------------------------

frame6 = { emptyFrame | title <- Just "Colored frame",
  backgroundColor <- Just yellow,
  headerBackgroundColor <- Just green,
  headerHeight <- Just 0.2,
  topMargin <- Just 0.25,
  leftMargin <- Just 0.2,
  content <- Just [markdown|This frame has the following fields set:

    title <- Just "Colored frame",
    backgroundColor <- Just yellow,
    headerBackgroundColor <- Just green,
    headerHeight <- Just 0.2,
    topMargin <- Just 0.25,
    leftMargin <- Just 0.2,

... and `content`.
|] }

--------------------------

frame7 = { emptyFrame | title <- Just "Selection box", selectionBoxColor <- Just blue,
  content <- Just [markdown|If the `selectionBoxColor` field is set for a frame, then you can use the mouse to select an area of the frame by pressing the left mouse button and moving the mouse while the button remains pressed.

Preselm will draw a rectangle on the frame using the color specified in by the field. That way, during the presentation, you can select an area on the frame to point it out to the audience.

Since this frame has the `selectionBoxColor` set to `Just blue`, you can try it now.
|] }

--------------------------

frame8 = { emptyFrame | middle <- Just [markdown|## You can find Preselm [here](https://github.com/grzegorzbalcerek/Preselm).|] }

--------------------------

frame9 = { emptyFrame | middle <- Just [markdown|## This the end of the tutorial.|] }

--------------------------

keys = (\x -> [x]) <~  Keyboard.lastPressed

handle : [Keyboard.KeyCode] -> Maybe Preselm.Action
handle keys = if | keys == [39] || keys == [78] || keys == [13] -> Just Preselm.Forward
                 | keys == [37] || keys == [80]                 -> Just Preselm.Backward
                 | keys == [35] || keys == [69]                 -> Just Preselm.End
                 | keys == [36] || keys == [72]                 -> Just Preselm.Home
                 | otherwise                                    -> Nothing

main = presentation [frame1, frame2, frame3, hwframe1, hwframe2, frame4, frame5, frame6, frame7, frame8, frame9] (constant (500, 700)) keys handle
