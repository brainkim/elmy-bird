import Keyboard
import Time
import Window
import Random

delta : Signal Float
delta = (\t -> t/20) <~ fps 60

type Dims = (Float, Float) 
type Input = (Float, Bool, Int, Dims) 

toFloat2 (w, h) = (toFloat w, toFloat h)

inputSig : Signal Input
inputSig = sampleOn delta ((,,,) <~ delta
                                  ~ Keyboard.space
                                  ~ Random.range -100 100 (every 2000) 
                                  ~ lift toFloat2 Window.dimensions)

data State = Waiting | Playing | Dead

type Pipe = { x: Float, top: Float, bottom: Float }
type Bird = { y: Float, dy: Float }
type Game = { pipes: [Pipe], countdown: Float, bird: Bird, state: State }

initialGame : Game
initialGame = { pipes = [],
                countdown = 30,
                bird = { y = 0, dy = 0 },
                state = Waiting }

updateCountdown (d, _, _, _) countdown = if countdown <= 0 then 100 else countdown - d

filterPipe (w, h) pipe = pipe.x > (-w / 2 - 50)
movePipe d pipe = { pipe | x <- pipe.x - 1.5 * d }

addPipe rand (w, h) game =
    if game.countdown <= 0
    then { x = w / 2 + 50, top = 100 + rand, bottom = -100 + rand} :: game.pipes
    else game.pipes

updatePipes (d, f, rand, dims) game =
    addPipe rand dims game
    |> map (movePipe d)
    |> filter (filterPipe dims)

gravity d bird = { bird | y <- max -300 (bird.y + bird.dy * d),
                         dy <- max -50 (bird.dy - 0.8 * d ^ 2)}

flap f bird = { bird | dy <- if f then 7 else bird.dy }

updateBird (d, f, _, _) bird = flap f bird |> gravity d

near : Float -> Float -> Float -> Bool
near n c m = m >= n - c && m <= n + c

collision : Bird -> Pipe -> Bool
collision bird pipe = near 12.5 25 pipe.x && (bird.y > pipe.top ||
                                           bird.y < pipe.bottom) 

updateState game = if any (collision game.bird) game.pipes
                   then Dead
                   else Playing

stepPlaying input game = { game | state     <- updateState game,
                                  countdown <- updateCountdown input game.countdown,
                                  pipes     <- updatePipes input game,
                                  bird      <- updateBird input game.bird }

continue flap = if flap then Waiting else Dead

stepDead ((d,flap,rand,dims) as input) game = { game | state <- continue flap,
                                                       bird  <- updateBird input game.bird } 

stepWaiting (_,flap,_,_) game = if flap
                                then { initialGame | state <- Playing } 
                                else initialGame
step : Input -> Game -> Game
step input game = case game.state of
                      Playing -> stepPlaying input game 
                      Dead -> stepDead input game
                      Waiting -> stepWaiting input game

gameSig = foldp step initialGame inputSig

pipeGreen = rgb 60 100 60

drawPipe h pipe =
    let topPipeHeight = abs (h / 2 - pipe.top)
        bottomPipeHeight = abs (-(h / 2) - pipe.bottom)
    in 
        [rect 50 topPipeHeight |> filled pipeGreen
        |> move (pipe.x, h / 2 - topPipeHeight / 2)
        ,rect 50 bottomPipeHeight |> filled pipeGreen
        |> move (pipe.x, -(h / 2) + bottomPipeHeight / 2)]

flappyColor = rgb 255 0 0
displayBird bird = oval 25 25 |> filled flappyColor |> move (0, bird.y)

display (w, h) game = collage w h <| displayBird game.bird ::
                                     (concatMap (drawPipe (toFloat h))
                                                 game.pipes)

main = lift2 display Window.dimensions gameSig
