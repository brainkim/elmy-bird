import Keyboard
import Mouse
import Time
import Window
import Random

-- Constants
gameWidth = 288
gameHeight = 512
overflow = 50
gravity = -0.9
pipeInterval = 3

-- Input
delta : Signal Float
delta = (\t -> t/20) <~ fps 60

type Inputs = {time: Float, tap: Bool, rand: Int}
inputSig : Signal Inputs
inputSig = Inputs <~ delta
                   ~ dropRepeats Keyboard.space
                   ~ Random.range -100 100 (every 1000)

-- Model
data State = Waiting | Playing | Dead
type Object = { x: Float, y: Float, width: Float, height: Float }
type Moving a = { a | vx: Float, vy: Float }
type Bird = Moving Object
type Pipe = Moving Object
type Model = 
    { pipes: [Pipe]
    , countdown: Float
    , bird: Bird
    , state: State
    , score: Int }

bird : Bird
bird = { x=0, y=0, vx=0, vy=0, width=92/3, height=64/3 }

initial : Model
initial =
    { pipes = []
    , countdown = 4 
    , bird = bird
    , state = Waiting
    , score = 0 }

-- Update
-- Physics
moving : Inputs -> Moving Object -> Moving Object
moving {time} m =
    { m | x <- m.x + m.vx * time
        , y <- m.y + m.vy * time }

falling : Inputs -> Moving Object -> Moving Object
falling {time} f = { f | vy <- f.vy + gravity * time ^ 2 }

nearing : Float -> Float -> Float -> Bool
nearing n c m = m >= n - c && m <= n + c

-- colliding : Object -> Object -> Bool
colliding : Bird -> Pipe -> Bool
colliding obj1 obj2 =
    nearing obj1.x (obj1.width/2 + obj2.width/2) obj2.x ||
    nearing obj1.y (obj1.height/2 + obj2.height/2) obj2.y

-- Game
pipe : Pipe
pipe = { x=3, y=0, vx=-10, vy=0, width=500, height=500 }

data Position = Top | Bottom
createPipe : Int -> Position -> Pipe
createPipe rand pos = pipe

addPipes : Inputs -> Model -> [Pipe]
addPipes {rand} game =
    if game.countdown <= 0
    then createPipe rand Top :: createPipe rand Bottom :: game.pipes
    else game.pipes

filterPipe : Pipe -> Bool
filterPipe pipe = pipe.x > -gameWidth/2 - overflow

updatePipes : Inputs -> Model -> [Pipe]
updatePipes inputs game =
    addPipes inputs game
    |> map (moving inputs)
    |> filter filterPipe

flapping : Inputs -> Bird -> Bird
flapping {tap} bird = { bird | vy <- if tap then 7 else bird.vy }

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
    if any (\n -> nearing n.x 0.1 0) game.pipes
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

drawBird bird =
    collage 500 500
    [(toForm (fittedImage (round bird.width + 1) (round bird.height + 1) "assets/animation.gif")
     |> move (0, bird.y))]

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
