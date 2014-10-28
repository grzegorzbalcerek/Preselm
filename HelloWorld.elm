import Graphics.Input as Input

import Preselm (..)

input : Input.Input (Maybe Action)
input = Input.input Nothing

next = Input.button input.handle (Just Forward) "Next!"
back = Input.button input.handle (Just Backward) "Back!"


frame1 = { emptyFrame | middle <- Just [markdown|# Hello World!|] }

frame2 = { emptyFrame | title <- Just "Goodbye", middle <- Just [markdown|#World!|] }

scene x = layers [flow right [next, back], x]

main = scene <~ (presentation [frame1, frame2] (constant (1000, 1000)) input.signal id)
