import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Window
import Mouse
import Html exposing (..)
import Html.Attributes exposing (style)

type alias Vector =
    { x : Float
    , y : Float
    }

(<+>) : Vector -> Vector -> Vector
(<+>) l m = Vector (l.x + m.x) (l.y + m.y)

(<*>) : Float -> Vector -> Vector
(<*>) k l = Vector (k * l.x) (k * l.y)

vzero : Vector
vzero = Vector 0 0

-- Gravity
g : Vector
g = Vector 0 -1000

-- Ball has mass, radius, coordinates, velocity and acceleration
type alias Ball = 
    { m : Float
    , r : Float
    , q : Vector 
    , v : Vector
    , a : Vector
    }

-- Room has height, width, wall thickness, elasticity, q-factor and tension
type alias Room = 
    { h : Float
    , w : Float
    , th : Float
    , k : Float
    , qFactor : Float
    , u : Float
    }

-- Hand has position, grabbing state, elasticity and q-factor
type alias Hand =
    { q : Vector
    , isGrabbing : Bool
    , k : Float
    , qFactor : Float  
    }

type alias Model =
    { ball : Ball
    , room : Room
    , hand : Hand
    }

initial : Model
initial = 
    { ball = Ball 1 25 vzero (Vector 500 1000) g
    , room = Room 0 0 20 10000 10 0.02
    , hand = Hand vzero False 500 2
    }

-- apply function f to x n times
iterate : Int -> (a -> a) -> a -> a
iterate n f x =
  case n of
    0 -> x
    _ -> iterate (n - 1) f (f x)


update : Input -> Model -> Model
update {dims, mousePos, isDown, delta} ({ball, room, hand} as model) =
  let
    newRoom = updateRoom dims model.room
    newHand =
      { hand | 
          q = 
            { x = toFloat (fst mousePos) - (newRoom.w/2 + newRoom.th)
            , y = (newRoom.h/2 + newRoom.th) - toFloat (snd mousePos)
            },
          isGrabbing = isDown
      }
    dt = 0.001
  in
    { model |
        ball = model.ball
          |> iterate (round (delta / dt))
            (  moveBall dt
            >> setForces newRoom newHand dt
            >> accelerate dt
            ),
        room = newRoom,
        hand = newHand
    }

updateRoom : (Int, Int) -> Room -> Room
updateRoom (w, h) room =
  { room |
      w = toFloat w - 2 * room.th,
      h = toFloat h - 2 * room.th
  }

resist {k, qFactor} = sqrt k / qFactor

normal : Room -> Float -> Float -> Float
normal room dist v =
  if dist < 0 then 0 else -room.k * dist - resist room * v

sign : Float -> Float
sign x =
  if x > 0 then 1
  else if x < 0 then -1
  else 0

absmin : Float -> Float -> Float
absmin x y = min (abs x) (abs y)

tension : Float -> Vector -> Vector -> Vector
tension u {x, y} n =
  { x = -(sign x) * absmin x (u * n.y)
  , y = -(sign y) * absmin y (u * n.x)
  }

elastic : Hand -> Vector -> Vector -> Vector
elastic ({q, isGrabbing, k} as hand) q' v =
  case isGrabbing of
    False -> vzero
    True ->
      let
        dq = -1 <*> q' <+> q
        r = -(resist hand)
      in
        (k <*> dq) <+> (r <*> v) 

setForces : Room -> Hand -> Time -> Ball -> Ball
setForces room hand dt ({q, v, r} as ball) =
  let
    fHand = elastic hand q v
    fNormal =
      { x = normal room (q.x + r - room.w/2) v.x - normal room (-q.x + r - room.w/2) -v.x
      , y = normal room (q.y + r - room.h/2) v.y - normal room (-q.y + r - room.h/2) -v.y
      }
    fTension = tension room.u ((1/dt) <*> v) fNormal
  in
    { ball | a = g <+> fHand <+> fNormal <+> fTension}

moveBall : Time -> Ball -> Ball
moveBall dt ball =
  { ball | q = dt <*> ball.v <+> ball.q }

accelerate : Time -> Ball -> Ball
accelerate dt ball =
  { ball | v = dt <*> ball.a <+> ball.v }


view : Model -> Html
view ({ball, room, hand} as model) =
  let
    w = round <| room.w + 2 * room.th
    h = round <| room.h + 2 * room.th
    wall = solid gray
  in
    div 
      [ style
        [ ("cursor", if hand.isGrabbing then "-webkit-grabbing" else "-webkit-grab") ]
      ]
      [ fromElement <|
          collage w h
            [ rect (room.w + room.th) (room.h + room.th)
                |> outlined { wall | width = room.th, cap = Padded }
            , circle ball.r
                |> filled red
                |> move (ball.q.x, ball.q.y)
            ]
      ]

main : Signal Html
main = Signal.foldp update initial input
  |> Signal.map view

type alias Input =
    { dims : (Int, Int)
    , mousePos : (Int, Int)
    , isDown : Bool
    , delta : Time
    }

delta : Signal Time
delta = Signal.map inSeconds (fps 60)

input : Signal Input
input = 
  Signal.sampleOn delta <|
    Signal.map4 Input
      Window.dimensions
      Mouse.position
      Mouse.isDown
      delta


