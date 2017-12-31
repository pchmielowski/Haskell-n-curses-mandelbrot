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

scaled n zoom = ((fromIntegral n) / (fromIntegral zoom))

isShaded :: Integer -> Integer -> Integer -> Bool
isShaded x y zoom = (color ((scaled x zoom) :+ (scaled y zoom))) >= max_iterations

point :: Integer -> Integer -> Integer -> Point
point x y zoom = Point x y (isShaded x y zoom)

figure zoom = concat $ map (\x -> map (\y -> point x y zoom) [0..100]) [0..50]

drawPoint :: Point -> Update ()
drawPoint p = do
    moveCursor (x p) (y p)
    drawString (if shaded p then "#" else " ")

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    waitFor w

data Position = Position { zoom :: Integer }
changeScale position delta = position { zoom = (zoom position) + delta }
scaleUp position = changeScale position 10
scaleDown position = changeScale position (-10)

waitFor :: Window -> Curses ()
waitFor w = loop $ Position 50 where
    loop position = do
        updateWindow w $ do
            mapM_ drawPoint $ figure $ zoom position
        render
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop position
            Just (EventCharacter '+') -> loop $ scaleUp position
            Just (EventCharacter '-') -> loop $ scaleDown position
            Just ev' -> if (ev' == EventCharacter 'q') then return () else loop position
