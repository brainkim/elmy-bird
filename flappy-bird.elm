import Keyboard
import Mouse
import Time
import Window
import Random

-- Input
delta : Signal Float
delta = (\t -> t/20) <~ fps 60

type Inputs = {time: Float, tap: Bool, rand: Int}
inputSig : Signal Inputs
inputSig = sampleOn delta (Inputs <~ delta
                                   ~ Keyboard.space
                                   ~ Random.range -250 250 (every 1000))

-- Model
-- Constants
gameWidth = 768
groundHeight = 128
groundY = (toFloat -backgroundHeight) / 2 - groundHeight
groundTop = groundY + (toFloat groundHeight) / 2 - 1
backgroundHeight = 896
gameHeight = backgroundHeight + groundHeight
pipeWindow = 900
birdX = -120
birdWidth = 92
birdHeight = 64
pipeWidth = 138
pipeHeight = 793
overflowX = 100
gravity = -1.2
flapVelocity = 12
pipeInterval = 70
gameScale = 0.8

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
bird = { x=birdX, y=0, vx=0, vy=0, width=birdWidth, height=birdHeight }

pipe : Pipe
pipe =
    { x = 400
    , y = 0
    , vx = -6
    , vy = 0
    , width = 138
    , height = 793
    , pos = Top }

initial : Model
initial =
    { pipes = []
    , countdown = 100 
    , bird = bird
    , state = Waiting
    , score = 0 }

-- Update
moving {time} m =
    { m | x <- m.x + m.vx * time
        , y <- m.y + m.vy * time }

falling {time} f = { f | vy <- f.vy + gravity * time^2 }

gap : Float
gap = pipeHeight / 2 + 120
createPipe : Int -> Position -> Pipe
createPipe rand pos =
    let y = case pos of
                Top -> toFloat rand + gap
                Bottom -> toFloat rand - gap
    in
    { pipe | y <- y
           , pos <- pos }

addPipes : Inputs -> Model -> [Pipe]
addPipes {rand} {countdown, pipes} =
    if countdown <= 0
    then [createPipe rand Top, createPipe rand Bottom] ++ pipes
    else pipes

filterPipe : Pipe -> Bool
filterPipe pipe = pipe.x > (toFloat -gameWidth) / 2 - overflowX

updatePipes : Inputs -> Model -> [Pipe]
updatePipes inputs game =
    addPipes inputs game
    |> map (moving inputs)
    |> filter filterPipe

flapping : Inputs -> Bird -> Bird
flapping {tap} bird = { bird | vy <- if tap then flapVelocity else bird.vy }

moveBird : Inputs -> Bird -> Bird
moveBird inputs bird =
    let bird' = moving inputs bird
    in
    { bird | y <- max (groundTop + toFloat bird.height/2 - 3) bird'.y }

updateBird : Inputs -> Model -> Bird
updateBird inputs = falling inputs . moveBird inputs . flapping inputs . .bird

nearing : Float -> Float -> Float -> Bool
nearing n c m = m >= n - c && m <= n + c

colliding obj1 obj2 =
    let width1 = toFloat obj1.width
        width2 = toFloat obj2.width
        height1 = toFloat obj1.height
        height2 = toFloat obj2.height
    in
    nearing obj1.x (width1/2 + width2/2 - 2) obj2.x &&
    nearing obj1.y (height1/2 + height2/2 - 2) obj2.y

grounded {y} = y >= (-gameHeight/2)

updateState : Model -> State
updateState {bird, pipes} =
    if any (colliding bird) pipes
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
           , bird      <- updateBird inputs game
           , pipes     <- updatePipes inputs game }

dead : Inputs -> Model -> Model
dead ({tap} as inputs) game =
    if tap
    then initial
    else { game | bird <- (falling inputs . moveBird inputs) game.bird }

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
birdTilt vy =
    clamp (degrees -90) (degrees 50) (8 * degrees vy)

drawGround =
    tiledImage gameWidth groundHeight "assets/ground.png"
    |> toForm
    |> moveY groundY

drawBackground =
    image gameWidth gameHeight "assets/background.png"
    |> toForm

drawBird bird =
    let asset = if bird.vy >= -5
                then "assets/flapping.gif"
                else "assets/falling.png"
    in
      image bird.width bird.height asset
      |> toForm
      |> move (bird.x, bird.y)
      |> rotate (birdTilt bird.vy)

drawPipe pipe =
    image pipe.width pipe.height "assets/pipe.png"
    |> toForm
    |> move (pipe.x, pipe.y)
    |> rotate (case pipe.pos of
                   Top -> pi
                   Bottom -> 0)

drawPipes pipes = []
    
draw (w, h) game = 
    (container w h middle . container gameWidth gameHeight middle . collage gameWidth gameHeight) <|
    [scale 0.6 <|group ([drawBackground, drawGround, drawBird game.bird,
                       (toForm . asText) game.state ] ++ (map drawPipe game.pipes))]

main = lift2 draw Window.dimensions gameSig
