import Window
import Keyboard

delta : Signal Float
delta = (\t -> t/20) <~ fps 60

-- noRepeats signal = foldp (\p -> \n -> if p == True then False else n) signal
type Input = Signal (Float, Bool)
input = sampleOn delta ((,) <~ delta ~ Keyboard.space)


type Bird = { y: Float, dy: Float }

initialGame : { flappy: Bird }
initialGame = { flappy = { y = 0, dy = -1 } } 

gravity d flappy = { flappy | y <- max -500 (flappy.y + flappy.dy * d),
                             dy <- max -50 (flappy.dy - 0.8 * d ^ 2)}
flap f flappy = { flappy | dy <- if f then 5 else flappy.dy }
stepFlappy (d, f) = gravity d . flap f
step input game = { game | flappy <- stepFlappy input game.flappy }

flappyColor = rgb 255 0 0
display game = collage 1000 1000 [oval 15 15 |> filled flappyColor |>
                                move (0, game.flappy.y)]
main = display <~ foldp step initialGame input
