module Penguin where

import Assets (..)

type Location =
    { x : Float
    , y : Float
    }

{-
  A Penguin has a location,
  Angle for Right Arm,
  Angle for Left Arm,
  and focus point where to look
-}
type Penguin = 
    { location : Location
    , rightArmAngle : Float
    , leftArmAngle : Float
    , focus : Location
    }

-- Convenience for left / right side of a penguin
data Side = Left | Right

-- The right / left arm given a Penguin
arm : Side -> Penguin -> Form
arm side {location, rightArmAngle, leftArmAngle} =
    let offset = { x = 130, y = -10 } in
    case side of
      Left -> move (location.x - offset.x, location.y + offset.y) 
              <| move (-10, 70) 
              <| rotate (degrees leftArmAngle) 
              <| group [move (10, -70) penguinLeftArm]
      Right -> move (location.x + offset.x, location.y + offset.y)
               <| move (10, 70)
               <| rotate (degrees rightArmAngle)
               <| group [move (-10, -70) penguinRightArm]

-- the Right / Left eye given a Penguin
eye : Side -> Penguin -> Form
eye side { location, focus } =
    let focusy' = (focus.y - (location.y + 70))/20
        focusx' = (focus.x - location.x)/20
        driftx = max -15 (min 15 focusx')
        drifty = max -5 (min 15 focusy')
        center = { x = location.x + driftx, y = location.y + 70 + drifty } 
        form = circle 12 |> filled black
        f = (flip move) form
    in case side of
      Left -> f (center.x - 20, center.y)
      Right -> f (center.x + 20, center.y)
             

-- The Body given a Penguin
body : Penguin -> Form
body { location } =
    move (location.x, location.y) penguinBody

-- Given a Penguin, turn it into a Form
draw : Penguin -> Form
draw penguin =
    group << apply penguin <| [ body
                              , eye Right
                              , eye Left
                              , arm Right
                              , arm Left
                              ]

apply : a -> [(a -> b)] -> [b]
apply p fs =
    case fs of 
      [] -> []
      (f::fs') -> (f p)::(apply p fs')