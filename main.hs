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

scaled n = ((fromIntegral n) / 50)

isShaded :: Integer -> Integer -> Bool
isShaded x y = (color ((scaled x) :+ (scaled y))) >= max_iterations

point :: Integer -> Integer -> Point
point x y = Point x y (isShaded x y)

figure = concat $ map (\x -> map (\y -> point x y) [0..100]) [0..50]

drawPoint :: Point -> Update ()
drawPoint p = do
    moveCursor (x p) (y p)
    drawString (if shaded p then "#" else " ")

main :: IO ()
main = runCurses $ do
    setEcho False
    color <- newColorID ColorWhite ColorBlack 1
    w <- defaultWindow
    updateWindow w $ do
        setColor color
        mapM_ drawPoint figure
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop