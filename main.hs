import UI.NCurses

data Point = Point { x :: Integer, y :: Integer, shaded :: Bool }

point :: Integer -> Integer -> Point
point x y = Point x y ((x + y) > 100)

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