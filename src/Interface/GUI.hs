-- | ???
--
-- Окно всегда 3 на 4.
-- Все сделано под базовый размер окна 640 на 480,
-- потому что так при масшатбировании оно лучше смотрится.
-- И потому, что так не нужно заново подбирать волшебные константы, 
-- отвечающие за положение объектов интерфейса.
module Interface.GUI where

import Core
import Types
import Graphics.Gloss.Interface.Pure.Game

-- | ???
windowWidth = 800

-- | ???
windowHeight = windowWidth*3/4

-- | ???
scaleFactor = windowHeight/480

-- | Константа @'gridNumber'@ определяет количество клеток.
-- На данный момент скорее для красоты.
-- Потому что она не поддерживается core-частью программы.
gridNumber = 8

-- | ???
gNumber = ceiling(gridNumber)

-- | ???
gridSize = 480/(gridNumber+2)

-- | ???
realGridSize = gridSize*scaleFactor

-- | ???
circleRadius = gridSize/2.2

-- | Основная функция, которая запускает графику.
interface :: IO ()
interface = do
  play display bgColor fps (initGame) drawWorld handleWorld updateWorld
  where
    windowSize   = (ceiling windowWidth, ceiling windowHeight)
    windowOffset = (200, 200)
    display = InWindow "Reversi" windowSize windowOffset
    bgColor = white
    fps = 60

-- | Функция, которая рисует все.
drawWorld::State->Picture
drawWorld s = scale scaleFactor scaleFactor
    (pictures 
    [ forAll (1, 1) (board s),
    showGameOver (player s),
    translate ( 270) ( 200) (drawChecker (player s)),
    translate (-250) ( 200) (drawChecker White),
    translate (-280) ( 120) (scale 0.5 0.5 (Text (show (fst(score s))))),
    translate (-250) (-60 ) (drawChecker Black),
    translate (-280) (-140) (scale 0.5 0.5 (Text (show (snd(score s))))) ])

-- | Если игра окончена, эта функция выводит соответствующую надпись на экран.
showGameOver::Player->Picture
showGameOver Empty = translate (-100) (-220)
  (scale 0.3 0.3 (text "Game Over!"))
showGameOver _ = Blank

-- | Функция, которая разбирает доску на клеточки.
forAll::Cell->Field->Picture
forAll (x, y) ((t:ts):ls)
  | (x == gNumber) && (y == gNumber) = (drawSquare t (gNumber, gNumber))
  | (x == gNumber) = pictures [(drawSquare t (gNumber, y)),(forAll (1, (y+1)) ls)]
  | otherwise = pictures [(drawSquare t (x, y)),(forAll ((x+1), y) (ts:ls))]

-- | Функция, которая рисует одну клетку доски в нужном месте картинки.
drawSquare::Player->Cell->Picture
drawSquare p (x, y) = translate
  (gridSize*(fromIntegral x-gridNumber/2))
  (gridSize*(fromIntegral y-gridNumber/2))
  (pictures [Color black (rectangleWire gridSize gridSize), (drawChecker p)])

-- | Функция, которая рисует одну шашку соответствующего цвета.
drawChecker::Player->Picture
drawChecker White = Color black (circle circleRadius)
drawChecker Black = Color black (circleSolid circleRadius)
drawChecker _ = Blank

-- | Функция, которая реагирует на нажатие мыши.
-- При нажатии на поле пытается сделать ход в эту клетку поля.
handleWorld::Event->State->State
handleWorld (EventKey (MouseButton LeftButton) Down _ (x, y)) s = 
  nextMoveGraphic s 
  (mov s 
    ((ceiling (y/realGridSize + gridNumber/2 - 0.5)),
     (ceiling (x/realGridSize + gridNumber/2 - 0.5))))
handleWorld _ s = s

-- | Функция, которая смотрит, "сделался" ли ход
-- и если да, то делает его.
nextMoveGraphic::State->Maybe State->State
nextMoveGraphic prev Nothing = prev
nextMoveGraphic _ (Just next)
  | (canMov next) = next
  | (not (canMov next)) && (canMov (switchMove next)) = switchMove next
  | otherwise = (gameOver next)

-- | Функция, которая обнуляет текущего игрока как знак того, что игра окончена.
gameOver::State->State
gameOver s = State f Empty sc
    where
        f = (board s)
        sc = (score s)

-- | Функция, которая теоретически может обновлять мир по времени, но ничего не делает.
updateWorld :: Float -> State -> State
updateWorld _ = id
