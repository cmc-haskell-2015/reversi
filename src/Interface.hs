module Interface where
import Core
import Main
import Graphics.Gloss.Interface.Pure.Game

type World = Move

interface :: IO ()
interface = do
  play display bgColor fps (initialize) drawWorld handleWorld updateWorld
  where
    windowSize   = (640, 480)
    windowOffset = (200, 200)
    display = InWindow "Reversi" windowSize windowOffset
    bgColor = white
    fps = 60

drawWorld (field,_,_,_) = forAll 1 1 drawSquare field
--pictures [Color black	(rectangleWire  400 100)]

forAll::Int->Int->(Player->Int->Int->Picture)->[[Player]]->Picture
forAll 8 8 f ((t:ts):ls) = pictures [(drawSquare t 8 8)]
forAll 8 y f ((t:ts):ls) = pictures [(drawSquare t 8 y),(forAll 1 (y+1) f ls)]
forAll x y f ((t:ts):ls) = pictures [(drawSquare t x y),(forAll (x+1) y f (ts:ls))]

drawSquare::Player->Int->Int->Picture
drawSquare p x y = pictures [Translate (50*(fromIntegral x-4)) (50*(fromIntegral y-4)) (drawChecker p)]

drawChecker::Player->Picture
drawChecker White = pictures [Color black (rectangleWire 50 50), Color black (circle 25)]
drawChecker Black = pictures [Color black (rectangleWire 50 50), Color black (circleSolid 25)]
drawChecker _ = Color black (rectangleWire 50 50)

handleWorld (EventKey (MouseButton LeftButton) Down _ (x, y)) board = 
  nextMoveGraphic board (move board (ceiling (y/50 + 3.5)) (ceiling (x/50 + 3.5)))
handleWorld _ board = board

nextMoveGraphic::Move->Revert->Move
-- Инициализация следующего хода
nextMoveGraphic prev Nothing = prev
nextMoveGraphic _ (Just next) = next

updateWorld _ = id