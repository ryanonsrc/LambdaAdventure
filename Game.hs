module Game where
    import System.IO
    
    data Occupant = You | Monster | Bomb | Treasure deriving Show
    
    type Slot = Maybe Occupant 
    type FieldRow = [Slot]
    type Field = [FieldRow]

    sampleField = [[Nothing, Nothing, Nothing],
                   [Nothing, Just(You), Nothing],
                   [Nothing, Nothing, Nothing]]

    main :: IO ()
    main = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        let field = sampleField
        _ <- gameLoop field
        return ()

    gameLoop :: Field -> IO Field
    gameLoop field = do
        putStrLn "\ESC[2J"
        mapM_ print field
        c <- getChar
        field' <- turn field c
        unlessM (c == 'x') (gameLoop field') (return field')
        
        
    turn :: Field -> Char -> IO Field
    turn f c = case c of
        'w' -> return $ updateField 0 0 (Just Monster) f
        'a' -> return $ updateField 0 0 (Just Bomb) f
        's' -> return $ updateField 0 0 (Just Treasure) f
        'd' -> return $ updateField 0 0 Nothing f           
        'x' -> return f
        _ -> return f

    unlessM :: Monad m => Bool -> m a -> m a -> m a
    unlessM cond thenM elseM =
        if cond then elseM else thenM

    updateField :: Int -> Int -> Slot -> Field -> Field
    updateField x y val field = 
        take x field ++ [updateRow y val (field !! x)] ++ drop (x + 1) field
    
    updateRow :: Int -> Slot ->FieldRow -> FieldRow
    updateRow y val row = 
        take y row ++ [val] ++ drop (y + 1) row