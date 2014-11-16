module Assets where

import Graphics.Collage (toForm, scale)
import Graphics.Element (croppedImage)

sheet : String -> (Int, Int) -> (Int, Int) -> Form
sheet src tl (w, h) = toForm << croppedImage tl w h <| "assets/" ++ src

penguinSheet : (Int, Int) -> (Int, Int) -> Form
penguinSheet = sheet "Penguin.png"

penguinBody : Form
penguinBody = penguinSheet (125, 0) (350, 375)

penguinLeftArm : Form
penguinLeftArm = penguinSheet (0, 0) (105, 180)

penguinRightArm : Form
penguinRightArm = penguinSheet (0, 185) (105, 180)