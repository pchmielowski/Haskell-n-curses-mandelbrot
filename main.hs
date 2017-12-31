import UI.NCurses
import Data.Complex

max_iterations :: Integer
max_iterations = 100

iteration :: RealFloat a => Complex a -> Complex a -> Integer -> Integer
iteration z c i 
  | i > max_iterations = max_iterations
  | diverge z = i
  | otherwise = iteration ((z**2) + c) c (i + 1)

diverge :: RealFloat a => Complex a -> Bool
diverge z = magnitude z > 2.0

color :: RealFloat a => Complex a -> Integer
color point = iteration (0 :+ 0) point 0

scale :: Fractional a => a -> a
scale n = n / 200

points :: [Double]
points = map scale [-500..500]

data Point = Point { x :: Integer, y :: Integer, shaded :: Bool }

scaled n position zoom = ((fromIntegral (n - position)) / (fromIntegral zoom))

isShaded :: Integer -> Integer -> Position -> Bool
isShaded x y position = (color ((scaled x 0 (zoom position)) :+ (scaled y (posX position) (zoom position)))) >= max_iterations

point :: Integer -> Integer -> Position -> Point
point x y position = Point x y (isShaded x y position)

figure position = concat $ map (\x -> map (\y -> point x y $ position) [0..100]) [0..50]

drawPoint :: Point -> Update ()
drawPoint p = do
    moveCursor (x p) (y p)
    drawString (if shaded p then "#" else " ")

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    waitFor w

data Position = Position { posX :: Integer, zoom :: Integer }

changeScale position delta = position { zoom = (zoom position) + delta }
scaleUp position = changeScale position 10
scaleDown position = changeScale position (-10)

toRight position = position { posX = (posX position) + 1 }

waitFor :: Window -> Curses ()
waitFor w = loop $ Position 0 50 where
    loop position = do
        updateWindow w $ do
            mapM_ drawPoint $ figure $ position
        render
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop position
            Just (EventSpecialKey KeyRightArrow) -> loop $ toRight position 
            Just (EventCharacter '+') -> loop $ scaleUp position
            Just (EventCharacter '-') -> loop $ scaleDown position
            Just ev' -> if (ev' == EventCharacter 'q') then return () else loop position
