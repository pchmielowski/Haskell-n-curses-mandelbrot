import UI.NCurses

data Point = Point { x :: Integer, y :: Integer }

dot :: Point -> Update ()
dot p = do
    moveCursor (x p) (y p)
    drawString " "

figure = map (\y -> Point 0 y) $ filter (\n -> n `mod` 3 /= 0) [0..100]

main :: IO ()
main = runCurses $ do
    setEcho False
    color <- newColorID ColorWhite ColorWhite 1
    w <- defaultWindow
    updateWindow w $ do
        setColor color
        mapM_ dot figure
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop