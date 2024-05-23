module Main where
    import System.IO
    import Text.PrettyPrint.Annotated.HughesPJ (renderDecorated, render)
    import Text.Printf (printf)

    import System.Directory.Internal.Prelude (getArgs)
    import Data.Map (argSet)
    import Control.Monad
    import Control.Monad.Extra
    
    data Space = Empty | You | Monster | Bomb | Treasure
    data RenderMask = Mask { obfuscate :: Bool, string :: String }
    data Cursor = Location { col :: Int, row :: Int }

    type Row = [Space]
    type PlayField = [Row]

    data GameParameters = GameParameters {
        dimension :: Int,
        omnipotent :: Bool,
        builder :: Bool
    } deriving (Show)

    data GameState = GameState {
        playfield :: [Row],
        cursor :: Cursor
    }

    playMask = Mask { obfuscate = True, string = " %s " }
    buildMask = Mask { obfuscate = False, string = " %s "}
    buildCursorMask = Mask { obfuscate = False, string = "[%s]"}


    devMode :: GameParameters
    devMode = GameParameters { dimension = 7, omnipotent = True, builder = True }

    playaMode = GameParameters {dimension = 7, omnipotent = False, builder = False }

    samplePlayField :: [[Space]]
    samplePlayField = [[Empty, Empty, Empty],
                   [Empty, You, Empty],
                   [Empty, Empty, Empty]]

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
        let state = GameState { playfield = emptyPlayField (dimension params), cursor = Location {col = 0, row = 0} }
        -- let field = emptyPlayField (dimension params)
        state' <- ifM (return $ builder params) (builderLoop params state) (return state)
        _ <- gameLoop params state'
        return ()

    emptyPlayField :: Int -> PlayField
    emptyPlayField n = replicate n (replicate n Empty)


    gameLoop :: GameParameters -> GameState -> IO GameState
    gameLoop params state = do
        putStrLn "\ESC[2J"
        -- mapM_ print field
        putStrLn $ renderPlayField (not (omnipotent params)) state
        c <- getChar
        state' <- turn state c
        ifM (return $ c == 'x') (return state') (gameLoop params state')

    builderLoop :: GameParameters -> GameState -> IO GameState
    builderLoop params state = do
        putStrLn "\ESC[2J"
        putStrLn $ renderPlayField False state
        c <- getChar
        state' <- buildStep state c
        ifM (return $ c == 'x') (return state') (builderLoop params state')
    
    turn :: GameState -> Char -> IO GameState
    turn s c = case c of
        'w' -> return $ updatePlayField 0 0 Monster s
        'a' -> return $ updatePlayField 0 0 Bomb s
        's' -> return $ updatePlayField 0 0 Treasure s
        'd' -> return $ updatePlayField 0 0 Empty s           
        'x' -> return s
        _ -> return s

    buildStep :: PlayField -> Char -> IO PlayField
    buildStep = undefined

    updatePlayField :: Int -> Int -> Space -> GameState -> GameState
    updatePlayField x y val state = 
        take x (playfield state) ++ [updateRow y val ((playfield state) !! x)] ++ drop (x + 1) (playfield state)
    
    updateRow :: Int -> Space -> Row -> Row
    updateRow y val row = 
        take y row ++ [val] ++ drop (y + 1) row

    -- renderPlayField :: Bool -> GameState -> String
    -- renderPlayField obsfucate state = 
    --     unlines $ map (renderRow obsfucate (playfield state) (cursorRow state) (cursorCol state))
    renderPlayField :: Bool -> GameState -> String
    renderPlayField obsfucate state = 
        unlines $ map (\r -> renderRow obsfucate r (cursor state)) (playfield state)

    renderRow :: Bool -> Row -> Cursor -> String
    renderRow obsfucate r c = "| " ++ unwords (map (obsfucateSpace obsfucate . show) row) ++ " |"

    instance Show Space where
        show Empty = " ⠀⠀ "
        show You = " 😎 "
        show Monster = " 👹 "
        show Bomb = " 💣 "   
        show Treasure = " 💰 "

    renderSpace :: GameState -> RenderMask -> Space -> String
    renderSpace = undefined

    obsfucateSpace :: Bool -> String -> String
    obsfucateSpace enabled = map (if enabled then obsfucateChar else id)
        where obsfucateChar ch 
                | ch == '💣' = '❔'
                | ch == '👹' = '❔'
                | ch == '💰' = '❔'
                | otherwise = ch
