import Keyboard
import Mouse
import Time
import Window
import Random

-- Constants
gameWidth = 288
gameHeight = 512
overflow = 50
gravity = -9.75

-- Input
delta : Signal Float
delta = inSeconds <~ fps 35

type Inputs = {time: Float, tap: Bool, rand: Int}
inputSig : Signal Inputs
inputSig = sampleOn delta <| Inputs <~ delta
                                     ~ merge Keyboard.space Mouse.isClicked
                                     ~ Random.range -100 100 (every 2000)

-- Model
data State = Waiting | Playing | Dead
type Object a = { a | x: Float, y: Float, width: Float, height: Float }
type Moving a = { a | vx: Float, vy: Float }
type Bird = Moving (Object {})
type Pipe = Moving (Object {})
type Model = { pipes: [Pipe]
             , countdown: Float
             , bird: Bird
             , state: State }

initial : Model
initial = { pipes = []
          , countdown = 5 
          , bird = { x=0, y=0, vx=0, vy=0, width=25, height=25 }
          , state = Waiting }

-- Update
  -- Physics
moving : Inputs -> Moving (Object {}) -> Moving (Object {})
moving {time} m = { m | x <- m.x + m.vx * time
                      , y <- m.y + m.vy * time }

falling : Inputs -> Moving (Object {})
                 -> Moving (Object {})
falling {time} f = { f | vy <- f.vy + gravity * time ^ 2 }
  -- Game
filterPipe : Pipe -> Bool
filterPipe pipe = pipe.x > -gameWidth/2 - overflow

pipe : Pipe
pipe = { x=0, y=0, vx=0, vy=0, width=500, height=500 }
addPipe : Inputs -> Model -> [Pipe]
addPipe {rand} game =
    if game.countdown <= 0
    then { x = gameWidth/2 + overflow,
           y = 0,
           vx = -14,
           vy = 0,
           width = 500,
           height = 500 } :: game.pipes
    else game.pipes

updatePipes : Inputs -> Model -> [Pipe]
updatePipes inputs game =
    addPipe inputs game
    |> map (moving inputs)
    |> filter filterPipe

flapBird {tap} bird = { bird | vy <- if tap then 7 else bird.vy }

updateBird : Inputs -> (Bird -> Bird)
updateBird inputs = moving inputs . flapBird inputs

{--
near : Float -> Float -> Float -> Bool
near n c m = m >= n - c && m <= n + c

collision : Bird -> Pipe -> Bool
collision bird pipe = (near 12.5 25 pipe.x || near -12.5 25 pipe.x)
                      && (bird.y > pipe.top || bird.y < pipe.bottom) 

updateState game = if any (collision game.bird) game.pipes
                   then Dead
                   else Playing

stepPlaying input game = { game | state     <- updateState game,
                                  countdown <- updateCountdown input game.countdown,
                                  pipes     <- updatePipes input game,
                                  bird      <- updateBird input game.bird }

continue flap = if flap then Waiting else Dead

stepDead input game = { game | state <- continue input.flap,
                               bird  <- updateBird input game.bird }

stepWaiting input game = if input.flap
                         then { initialGame | state <- Playing } 
                         else initialGame

step : Input -> Game -> Game
step input game = case game.state of
                      Playing -> stepPlaying input game 
                      Dead -> stepDead input game
                      Waiting -> stepWaiting input game

gameSig = foldp step initialGame inputSig

-- Render
pipeGreen = rgb 60 100 60
drawBackground = toForm <| image 288 512 "assets/background_day.png"

drawPipe h pipe =
    let topPipeHeight = abs (h / 2 - pipe.top)
        bottomPipeHeight = abs (-(h / 2) - pipe.bottom)
    in 
        [rect 50 topPipeHeight |> filled pipeGreen
        |> move (pipe.x, h / 2 - topPipeHeight / 2)
        ,rect 50 bottomPipeHeight |> filled pipeGreen
        |> move (pipe.x, -(h / 2) + bottomPipeHeight / 2)]

flappyColor = rgb 255 0 0
displayBird bird =
  toForm (image 40 40 "assets/animation.gif")
    |> move (0, bird.y)
    |> rotate (4 * degrees bird.vy)

display game = collage <| displayBird game.bird ::
                          drawBackground
                          (concatMap (drawPipe (toFloat h))
                                      game.pipes)
--}
