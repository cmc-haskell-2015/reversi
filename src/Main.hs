module Main where
import Core
import Types
import Graphics.Gloss.Interface.Pure.Game

type World = Move

-- окно всегда 3 на 4
-- все сделано под базовый размер окна 640 на 480,
-- потому что так при масшатбировании оно лучше смотрится
-- и потому, что так не нужно заново подбирать волшебные константы, 
-- отвечающие за положение объектов интерфейса
windowWidth = 800
windowHeight = windowWidth*3/4
scaleFactor = windowHeight/480
gridNumber = 8
gridSize = 480/(gridNumber+2)
realGridSize = gridSize*windowHeight/480
circleRadius = gridSize/2

-- Основная функция, которая запускает графику
interface :: IO ()
interface = do
  play display bgColor fps (initialize) drawWorld handleWorld updateWorld
  where
    windowSize   = (ceiling windowWidth, ceiling windowHeight)
    windowOffset = (200, 200)
    display = InWindow "Reversi" windowSize windowOffset
    bgColor = white
    fps = 60

main :: IO ()
main = interface

-- Функция, которая рисует все
drawWorld::World->Picture
drawWorld (field,player,a,b) = scale scaleFactor scaleFactor
    (pictures 
    [ forAll 1 1 drawSquare field,
    showGameOver player,
    translate ( ( 270)) ( (200)) (drawChecker player),
    translate ( (-250)) ( (200)) (drawChecker White),
    translate ( (-280)) ( (120)) 
      (scale ( (0.5)) ( (0.5)) (Text (show b))),
    translate ( (-250)) ( (-60)) (drawChecker Black),
    translate ( (-280)) ( (-140)) 
      (scale ( (0.5)) ( (0.5)) (Text (show a))) ])

-- Если игра окончена, эта функция выводит соответствующую надпись на экран
showGameOver::Player->Picture
showGameOver Empty = translate (-100) (-220)
  (scale 0.3 0.3 (text "Game Over!"))
showGameOver _ = Blank

-- Функция, которая разбирает доску на клеточки  
forAll::Int->Int->(Player->Int->Int->Picture)->[[Player]]->Picture
forAll 8 8 f ((t:ts):ls) = (drawSquare t 8 8)
forAll 8 y f ((t:ts):ls) = pictures 
  [(drawSquare t 8 y),(forAll 1 (y+1) f ls)]
forAll x y f ((t:ts):ls) = pictures 
  [(drawSquare t x y),(forAll (x+1) y f (ts:ls))]

-- Функция, которая рисует одну клетку доски в нужном месте картинки
drawSquare::Player->Int->Int->Picture
drawSquare p x y = translate
  (gridSize*(fromIntegral x-gridNumber/2))
  (gridSize*(fromIntegral y-gridNumber/2))
  (pictures [Color black (rectangleWire gridSize gridSize), (drawChecker p)])

-- Функция, которая рисует одну шашку соответствующего цвета
drawChecker::Player->Picture
drawChecker White = Color black (circle circleRadius)
drawChecker Black = Color black (circleSolid circleRadius)
drawChecker _ = Blank

-- Функция, которая реагирует на нажатие мыши
handleWorld::Event->World->World
handleWorld (EventKey (MouseButton LeftButton) Down _ (x, y)) board = 
  nextMoveGraphic board 
  (move board 
    (ceiling (y/realGridSize + gridNumber/2 - 0.5))
    (ceiling (x/realGridSize + gridNumber/2 - 0.5)))
handleWorld _ board = board

-- Функция, которая смотрит, "сделался" ли ход
nextMoveGraphic::Move->Revert->Move
nextMoveGraphic prev Nothing = prev
nextMoveGraphic _ (Just next)
  | (canMove next) = next
  | (not (canMove next)) && (canMove (switchPlayer next)) = switchPlayer next
  | otherwise = (gameOver next)

-- Функция, которая обнуляет текущего игрока как знак того, что игра окончена  
gameOver::Move->Move
gameOver (f, p, a, b) = (f, Empty, a, b)

-- Функция, которая теоретически может обновлять мир по времени, но ничего не делает
updateWorld :: Float -> World -> World
updateWorld _ = id