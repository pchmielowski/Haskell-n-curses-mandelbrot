import UI.NCurses

hash = do
    -- setColor $ newColorID ColorRed ColorBlue 0
    moveCursor 0 0
    drawString "#"

main :: IO ()
main = runCurses $ do
    setEcho False
    color <- newColorID ColorRed ColorBlue 1
    w <- defaultWindow
    updateWindow w $ do
        setColor color
        hash
        moveCursor 1 10
        drawString "Hello world!"
        moveCursor 3 10
        drawString "(press q to quit)"
        moveCursor 0 0
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop