module Game where
    import System.IO
    
    data Occupant = You | Monster | Bomb | Treasure deriving Show
    
    type Space = Maybe Occupant 
    type Row = [Space]
    type PlayField = [Row]

    samplePlayField = [[Nothing, Nothing, Nothing],
                   [Nothing, Just(You), Nothing],
                   [Nothing, Nothing, Nothing]]

    main :: IO ()
    main = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        let field = samplePlayField
        _ <- gameLoop field
        return ()

    gameLoop :: PlayField -> IO PlayField
    gameLoop field = do
        putStrLn "\ESC[2J"
        mapM_ print field
        c <- getChar
        field' <- turn field c
        unlessM (c == 'x') (gameLoop field') (return field')
        
        
    turn :: PlayField -> Char -> IO PlayField
    turn f c = case c of
        'w' -> return $ updatePlayField 0 0 (Just Monster) f
        'a' -> return $ updatePlayField 0 0 (Just Bomb) f
        's' -> return $ updatePlayField 0 0 (Just Treasure) f
        'd' -> return $ updatePlayField 0 0 Nothing f           
        'x' -> return f
        _ -> return f

    unlessM :: Monad m => Bool -> m a -> m a -> m a
    unlessM cond thenM elseM =
        if cond then elseM else thenM

    updatePlayField :: Int -> Int -> Space -> PlayField -> PlayField
    updatePlayField x y val field = 
        take x field ++ [updateRow y val (field !! x)] ++ drop (x + 1) field
    
    updateRow :: Int -> Space -> Row -> Row
    updateRow y val row = 
        take y row ++ [val] ++ drop (y + 1) row