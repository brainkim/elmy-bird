import Keyboard
import Window
import Random

delta : Signal Float
delta = (\t -> t/20) <~ fps 60

-- noRepeats signal = foldp (\p -> \n -> if p == True then False else n) signal
type Input = Signal (Float, Bool, Int)
input = sampleOn delta ((,,) <~ delta
                              ~ Keyboard.space
                              ~ Random.range -50 50)

data State = Waiting | Playing | Dead
type Pipe = { x: Float, top: Float, bottom: Float}
type Bird = { y: Float, dy: Float }
type Game = { pipes: [Pipe], countdown: Float }

initialGame : Game
initialGame = { pipes = [], countdown = 30}

updateCountdown d countdown = if countdown <= 0 then 100 else countdown - d

filterPipe pipe = pipe.x > -100

movePipe d pipe = { pipe | x <- pipe.x - d }

addPipe rand game =
    if game.countdown <= 0
    then { x = 200, top = 50 + rand, bottom = -50 + rand} :: game.pipes
    else game.pipes

updatePipes (d, rand) game =
    addPipe rand game
    |> map (movePipe d)
    |> filter filterPipe
stepGame (d, flaps, rand) game =
    { game | countdown <- updateCountdown d game.countdown,
                 pipes <- updatePipes (d, rand) game }

step : (Float, Bool, Int) -> Game -> Game
step input = stepGame input

pipeGreen = rgb 60 100 60
displayPipe pipe = [move (pipe.x, pipe.top) <| filled pipeGreen <|
                    rect 50 50 
                   ,move (pipe.x, pipe.bottom) <| filled pipeGreen <|
                    rect 50 50]

display game = collage 500 500 <| concatMap displayPipe game.pipes
gameState = foldp step initialGame input

main = lift display <| foldp step initialGame input
