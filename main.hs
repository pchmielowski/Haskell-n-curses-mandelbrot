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

data Point = Point { x :: Integer, y :: Integer, shade :: Integer }

scaled n position zoom = ((fromIntegral (n - position)) / (fromIntegral zoom))

intensity :: Integer -> Integer -> Position -> Integer
intensity x y position = color $ real :+ imag
    where 
        real = transform x posX
        imag = transform y posY
        transform n delta = (scaled n (delta position) $ zoom position)

point :: Integer -> Integer -> Position -> Point
point x y position = Point x y (intensity x y position)

figure position = concat $ map (\x -> map (\y -> point x y $ position) [0..100]) [0..50]

drawPoint :: Point -> Update ()
drawPoint p = do
    moveCursor (x p) (y p)
    drawString " "

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    waitFor w

data Position = Position { posX :: Integer, posY :: Integer, zoom :: Integer }

zoomDelta = 20
changeScale position delta = position { zoom = (zoom position) + delta }
scaleUp position = changeScale position zoomDelta
scaleDown position = changeScale position (-zoomDelta)

positionDelta = 20
toRight position = position { posY = (posY position) + positionDelta }
toDown position = position { posX = (posX position) + positionDelta }
toLeft position = position { posY = (posY position) - positionDelta }
toUp position = position { posX = (posX position) - positionDelta }

drawColorPoint :: Point -> Window -> Curses ()
drawColorPoint point win = do 
    defineColor (Color $ fromIntegral $ shade point) colorIntensity colorIntensity colorIntensity
    color <- newColorID ColorRed (Color $ fromIntegral $ shade point) (fromIntegral $ shade point)
    updateWindow win $ do
        setColor color
        drawPoint point
    where colorIntensity = (fromIntegral $ shade point) * 10

waitFor :: Window -> Curses ()
waitFor w = loop $ Position 0 0 50 where
    loop position = do
        mapM_ (\p -> drawColorPoint p w) $ figure $ position
        render
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop position
            Just (EventSpecialKey KeyRightArrow) -> loop $ toRight position 
            Just (EventSpecialKey KeyDownArrow) -> loop $ toDown position 
            Just (EventSpecialKey KeyLeftArrow) -> loop $ toLeft position 
            Just (EventSpecialKey KeyUpArrow) -> loop $ toUp position 
            Just (EventCharacter '+') -> loop $ scaleUp position
            Just (EventCharacter '-') -> loop $ scaleDown position
            Just ev' -> if (ev' == EventCharacter 'q') then return () else loop position
