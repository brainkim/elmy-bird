import Keyboard
import Mouse
import Time
import Window
import Random

-- Constants
gameWidth : Int
gameWidth = 768
groundHeight : Int
groundHeight = 128
groundY = (toFloat -backgroundHeight) / 2 - (toFloat groundHeight) / 2
backgroundHeight : Int
backgroundHeight = 896
gameHeight = backgroundHeight + groundHeight
birdWidth = 92
birdHeight = 64
overflow = 50
gravity = -0.8
flapVelocity = 8
pipeInterval = 3

-- Input
delta : Signal Float
delta = (\t -> t/20) <~ fps 60

type Inputs = {time: Float, tap: Bool, rand: Int}
inputSig : Signal Inputs
inputSig = Inputs <~ delta
                   ~ Keyboard.space
                   ~ Random.range -100 100 (every 1000)

-- Model
data State = Waiting | Playing | Dead
type Object a =
    { a | x: Float
        , y: Float
        , vx: Float
        , vy: Float
        , width: Int
        , height: Int }

type Bird = Object {}
data Position = Top | Bottom
type Pipe = Object { pos : Position }
type Ground = Object {}
type Model =
    { pipes: [Pipe]
    , countdown: Float
    , bird: Bird
    , state: State
    , score: Int }

bird : Bird
bird = { x=-120, y=0, vx=0, vy=0, width=46, height=32 }

initial : Model
initial =
    { pipes = []
    , countdown = 4 
    , bird = bird
    , state = Waiting
    , score = 0 }

-- Update
-- Physics
moving {time} m =
    { m | x <- m.x + m.vx * time
        , y <- m.y + m.vy * time }

falling {time} f = { f | vy <- f.vy + gravity * time^2 }

nearing : Float -> Float -> Float -> Bool
nearing n c m = m >= n - c && m <= n + c

-- colliding : Object {} -> Object {} -> Bool
colliding : Bird -> Pipe -> Bool
colliding obj1 obj2 =
    let width1 = toFloat obj1.width
        width2 = toFloat obj2.width
        height1 = toFloat obj1.height
        height2 = toFloat obj2.height
    in
    nearing obj1.x (width1/2 + width2/2) obj2.x ||
    nearing obj1.y (height2/2 + height2/2) obj2.y

-- Game
pipe : Pipe
pipe =
    { x = toFloat gameWidth / 2 + overflow
    , y = 0
    , vx = -5
    , vy = 0
    , width = 500
    , height = 500
    , pos = Top }

createPipe : Int -> Position -> Pipe
createPipe rand pos =
    { pipe | pos <- pos
           , y   <- case pos of 
                        Top -> toFloat rand + 25
                        Bottom -> toFloat -rand - 25 }

addPipes : Inputs -> Model -> [Pipe]
addPipes {rand} game =
    if game.countdown <= 0
    then createPipe rand Top :: createPipe rand Bottom :: game.pipes
    else game.pipes

filterPipe : Pipe -> Bool
filterPipe pipe = pipe.x > (toFloat -gameWidth) / 2 - overflow

updatePipes : Inputs -> Model -> [Pipe]
updatePipes inputs game =
    addPipes inputs game
    |> map (moving inputs)
    |> filter filterPipe

flapping : Inputs -> Bird -> Bird
flapping {tap} bird = { bird | vy <- if tap then flapVelocity else bird.vy }

updateBird : Inputs -> Model -> Bird
updateBird inputs = falling inputs . moving inputs . flapping inputs . .bird

updateState : Model -> State
updateState game =
    if any (colliding game.bird) game.pipes
    then Dead
    else Playing

updateCountdown : Inputs -> Model -> Float
updateCountdown {time} game =
    if game.countdown > 0
    then game.countdown - time
    else pipeInterval

-- This seems so wrong
updateScore : Model -> Int
updateScore game =
    if any (\p -> nearing p.x 0.1 game.bird.x) game.pipes
    then game.score + 1
    else game.score

play : Inputs -> Model -> Model
play inputs game =
    { game | state     <- updateState game
           , score     <- updateScore game
           , countdown <- updateCountdown inputs game
           , bird      <- updateBird inputs game }

dead : Inputs -> Model -> Model
dead ({tap} as inputs) game =
    if tap
    then initial
    else { game | bird <- falling inputs game.bird }

wait : Inputs -> Model -> Model
wait ({tap} as inputs) game =
    if tap
    then play inputs game
    else game

step : Inputs -> Model -> Model
step inputs game =
    case game.state of
        Playing -> play inputs game
        Waiting -> wait inputs game
        Dead -> dead inputs game

gameSig : Signal Model
gameSig = foldp step initial inputSig

-- Render
pipeGreen = rgb 60 100 60

drawBackground = toForm <| image gameWidth gameHeight "assets/background_day.png"

birdTilt vy =
    clamp (degrees -90) (degrees 50) (8 * degrees vy)

ground =
    tiledImage gameWidth groundHeight "assets/ground.png"
background =
    image gameWidth gameHeight "assets/background.png"
drawBird bird =
    let asset = if bird.vy >= -5
                then "assets/flapping.gif"
                else "assets/falling.png"
    in
    collage gameWidth gameHeight
    [ toForm background,
      toForm ground
      |> moveY groundY,
      image bird.width bird.height asset
      |> toForm
      |> move (bird.x, bird.y)
      |> rotate (birdTilt bird.vy) ]

main = lift (drawBird . .bird) gameSig

{--
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
