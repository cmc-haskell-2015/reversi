module Interface where
import Core
import Main
import Graphics.Gloss.Interface.Pure.Game

type World = Move

interface :: IO ()
-- Основная функция, которая запускает графику
interface = do
  play display bgColor fps (initialize) drawWorld handleWorld updateWorld
  where
    windowSize   = (640, 480)
    windowOffset = (200, 200)
    display = InWindow "Reversi" windowSize windowOffset
    bgColor = white
    fps = 60

drawWorld::World->Picture
-- Функция, которая рисует все
drawWorld (field,player,a,b) = pictures 
    [ forAll 1 1 drawSquare field,
    showGameOver player,
    Translate 270 200 (drawChecker player),
    Translate (-250) 200 (drawChecker White),
    Translate (-280) 120 (Scale 0.5 0.5 (Text (show b))),
    Translate (-250) (-60) (drawChecker Black),
    Translate (-280) (-140) (Scale 0.5 0.5 (Text (show a))) ]

showGameOver::Player->Picture
-- Если игра окончена, эта функция выводит соответствующую надпись на экран
showGameOver Empty = Translate (-100) (-220)
  (Scale 0.3 0.3 (Text "Game Over!"))
showGameOver _ = Blank
  
forAll::Int->Int->(Player->Int->Int->Picture)->[[Player]]->Picture
-- Функция, которая разбирает доску на клеточки
forAll 8 8 f ((t:ts):ls) = (drawSquare t 8 8)
forAll 8 y f ((t:ts):ls) = pictures 
  [(drawSquare t 8 y),(forAll 1 (y+1) f ls)]
forAll x y f ((t:ts):ls) = pictures 
  [(drawSquare t x y),(forAll (x+1) y f (ts:ls))]

drawSquare::Player->Int->Int->Picture
-- Функция, которая рисует одну клетку доски в нужном месте картинки
drawSquare p x y = pictures 
  [Translate (50*(fromIntegral x-4)) (50*(fromIntegral y-4)) (drawChecker p)]

drawChecker::Player->Picture
-- Функция, которая непосредственно рисует одну клетку доски
drawChecker White = pictures 
  [Color black (rectangleWire 50 50), Color black (circle 25)]
drawChecker Black = pictures 
  [Color black (rectangleWire 50 50), Color black (circleSolid 25)]
drawChecker _ = Color black (rectangleWire 50 50)

handleWorld::Event->World->World
-- Функция, которая реагирует на нажатие мыши
handleWorld (EventKey (MouseButton LeftButton) Down _ (x, y)) board = 
  nextMoveGraphic board (move board (ceiling (y/50 + 3.5)) (ceiling (x/50 + 3.5)))
handleWorld _ board = board

nextMoveGraphic::Move->Revert->Move
-- Функция, которая смотрит, "сделался" ли ход
nextMoveGraphic prev Nothing = prev
nextMoveGraphic _ (Just next)
  | (canMove next) = next
  | (not (canMove next)) && (canMove (switchPlayer next)) = switchPlayer next
  | otherwise = (gameOver next)
  
gameOver::Move->Move
-- Функция, которая обнуляет текущего игрока как знак того, что игра окончена
gameOver (f, p, a, b) = (f, Empty, a, b)

updateWorld :: Float -> World -> World
-- Функция, которая теоретически может обновлять мир, но ничего не делает
updateWorld _ = id