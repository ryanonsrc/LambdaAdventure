module Game where
    import System.IO
    import Text.PrettyPrint.Annotated.HughesPJ (renderDecorated, render)
    import System.Directory.Internal.Prelude (getArgs)
    import Data.Map (argSet)
    
    data Occupant = You | Monster | Bomb | Treasure | Cursor deriving Show
    
    type Space = Maybe Occupant 
    type Row = [Space]
    type PlayField = [Row]

    data GameParameters = GameParameters {
        dimension :: Int,
        omnipotent :: Bool,
        builder :: Bool
    } deriving (Show)

    devMode :: GameParameters
    devMode = GameParameters { dimension = 7, omnipotent = True, builder = True }

    playaMode = GameParameters {dimension = 7, omnipotent = False, builder = False }

    samplePlayField :: [[Maybe Occupant]]
    samplePlayField = [[Nothing, Nothing, Nothing],
                   [Nothing, Just(You), Nothing],
                   [Nothing, Nothing, Nothing]]

    main :: IO ()
    main = do
        args <- getArgs
        let params = parseArgs args 
        play params
        
    -- TODO: add error checking on params

    parseArgs :: [String] -> GameParameters
    parseArgs args = GameParameters { 
        omnipotent = parseOmnipotent args,
        dimension = parseDimension args,
        builder = parseBuilder args
        }

    parseOmnipotent :: [String] -> Bool
    parseOmnipotent args = elem "--omnipotent" args
    
    parseDimension :: [String] -> Int
    parseDimension args 
        | elem "--small" args = 3
        | elem "--medium" args = 7
        | elem "--large" args = 11
        | otherwise = 7

    parseBuilder :: [String] -> Bool
    parseBuilder args = elem "--builder" args

    play :: GameParameters -> IO ()
    play params = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        let field = emptyPlayField (dimension params)
        field' <- unlessM (builder params) (builderLoop params field) (gameLoop params field)
        _ <- gameLoop params field'
        return ()

    emptyPlayField :: Int -> PlayField
    emptyPlayField n = replicate n (replicate n Nothing)


    gameLoop :: GameParameters -> PlayField -> IO PlayField
    gameLoop params field = do
        putStrLn "\ESC[2J"
        -- mapM_ print field
        putStrLn $ renderPlayField (not (omnipotent params)) field
        c <- getChar
        field' <- turn field c
        unlessM (c == 'x') (gameLoop params field') (return field')

    builderLoop :: GameParameters -> PlayField -> IO PlayField
    builderLoop params field = do
        putStrLn "\ESC[2J"
        putStrLn $ renderPlayField False field
        c <- getChar
        field' <- buildStep field c
        unlessM (c == 'x') (builderLoop params field') (return field')
         
    unlessM :: Monad m => Bool -> m a -> m a -> m a
    unlessM cond thenM elseM =
        if cond then elseM else thenM
    
    turn :: PlayField -> Char -> IO PlayField
    turn f c = case c of
        'w' -> return $ updatePlayField 0 0 (Just Monster) f
        'a' -> return $ updatePlayField 0 0 (Just Bomb) f
        's' -> return $ updatePlayField 0 0 (Just Treasure) f
        'd' -> return $ updatePlayField 0 0 Nothing f           
        'x' -> return f
        _ -> return f

    buildStep :: PlayField -> Char -> IO PlayField
    buildStep = undefined

    updatePlayField :: Int -> Int -> Space -> PlayField -> PlayField
    updatePlayField x y val field = 
        take x field ++ [updateRow y val (field !! x)] ++ drop (x + 1) field
    
    updateRow :: Int -> Space -> Row -> Row
    updateRow y val row = 
        take y row ++ [val] ++ drop (y + 1) row

    renderPlayField :: Bool -> PlayField -> String
    renderPlayField obsfucate field = 
        unlines $ map (renderRow obsfucate) field

    renderRow :: Bool -> Row -> String
    renderRow obsfucate row = "| " ++ unwords (map (obsfucateSpace obsfucate . renderSpace) row) ++ " |"

    renderSpace :: Space -> String
    renderSpace Nothing = "   "
    renderSpace (Just You) = " Y "
    renderSpace (Just Monster) = " M "
    renderSpace (Just Bomb) = " B "
    renderSpace (Just Treasure) = " T "
    renderSpace (Just Cursor) = " ^ "

    obsfucateSpace :: Bool -> String -> String
    obsfucateSpace enabled = map (if enabled then obsfucateChar else id)
        where obsfucateChar ch 
                | ch == 'B' = '*'
                | ch == 'M' = '*'
                | ch == 'T' = '*'
                | otherwise = ch