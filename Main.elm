module Main where

import Penguin (..)
import Playground (..)
import Playground.Input (..)
import Debug

data Action = None
            | RightArm
            | LeftArm

type State = { penguin : Penguin
             , action : Action
             }

initialState : State
initialState = 
    { penguin = 
          { location = { x = 0, y = 0 }
          , rightArmAngle = 0
          , leftArmAngle = 0
          , focus = { x = 0, y = 0 }
          }
    , action = None
    }

update : RealWorld -> Input -> State -> State
update { mouse } input state =
    case input of
      MouseDown -> if | state.action == None -> selectAction mouse state
                      | otherwise -> 
                          let penguin' = handleAction mouse state.action state.penguin
                          in { state | penguin <- penguin' }
      Click -> { state | action <- None }                          
      Passive t -> 
          let penguin = state.penguin
          in { state | 
               penguin <- { penguin |
                            focus <- mouse
                          }
             }
      _ -> state

selectAction : Location -> State -> State
selectAction { x , y } state =
    if | x > 0 -> { state | action <- RightArm }
       | otherwise -> { state | action <- LeftArm }

handleAction : Location -> Action -> Penguin -> Penguin
handleAction { x, y } action penguin =
    let cx = if | action == LeftArm -> -130
                | otherwise -> 130
        cy = 40
        x' = x - cx
        y' = y - cy
        theta' = if x' == 0 then 0 else atan (y'/x')
    in case action of
      LeftArm -> { penguin | leftArmAngle <- getAngle x' y' theta' }
      RightArm -> { penguin | rightArmAngle <- getAngle x' y' theta' }

getAngle : Float -> Float -> Float -> Float
getAngle x y theta =
    let theta' = theta * (180/pi)
    in if | x > 0 && y > 0 -> theta' + 90
          | x < 0 && y > 0 -> theta' - 90
          | x < 0 && y < 0 -> theta' - 90
          | x > 0 && y < 0 -> theta' + 90
          | otherwise -> theta' + 90

render : RealWorld -> State -> [Form]
render rw state = [draw state.penguin]

main : Signal Element
main = play { update = update
            , render = render
            , initialState = initialState
            }