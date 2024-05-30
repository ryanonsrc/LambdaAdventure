module Main where
    import System.IO
    import Text.PrettyPrint.Annotated.HughesPJ (renderDecorated, render)
    import Text.Printf (printf)

    import System.Directory.Internal.Prelude (getArgs)
    import Data.Map (argSet)
    import Control.Monad
    import Control.Monad.Extra

    data Space = Empty | You | Monster | Bomb | Treasure
    data RenderMask = Mask { obfuscate :: Bool, withoutCursor :: String, withCursor :: String }
    data Cursor = Location { col :: Int, row :: Int }
    type RenderLocation = Cursor

    type Row = [Space]
    type PlayField = [Row]

    data GameParameters = GameParameters {
        dimension :: Int,
        omnipotentEnabled :: Bool,
        builderEnabled :: Bool
    } deriving (Show)

    data GameState = GameState {
        params :: GameParameters,
        playfield :: [Row],
        cursor :: Cursor
    }

    playMask = Mask { obfuscate = True, withCursor = " %s ", withoutCursor = " %s " }
    buildMask = Mask { obfuscate = False, withCursor = "[%s]", withoutCursor = " %s "}

    effectiveMask :: RenderMask -> GameParameters -> RenderMask
    effectiveMask m params =
        Mask { obfuscate = not (omnipotentEnabled params) && obfuscate m, 
               withCursor = withCursor m, 
               withoutCursor = withoutCursor m }

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
        dimension = parseDimension args,
        omnipotentEnabled = parseOmnipotent args,
        builderEnabled = parseBuilder args
        }

    parseOmnipotent :: [String] -> Bool
    parseOmnipotent = elem "--omnipotent"

    parseDimension :: [String] -> Int
    parseDimension args
        | "--small" `elem` args = 3
        | "--medium" `elem` args = 7
        | "--large" `elem` args = 11
        | otherwise = 7

    parseBuilder :: [String] -> Bool
    parseBuilder = elem "--builder"

    play :: GameParameters -> IO ()
    play p = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        let state = GameState { playfield = emptyPlayField (dimension p), cursor = Location {col = 0, row = 0}, params = p }
        state' <- ifM (return $ builderEnabled (params state)) (builderLoop state) (return state)
        _ <- gameLoop state'
        return ()

    emptyPlayField :: Int -> PlayField
    emptyPlayField n = replicate n (replicate n Empty)


    gameLoop :: GameState -> IO GameState
    gameLoop state = do
        putStrLn "\ESC[2J"
        -- mapM_ print field
        putStrLn $ renderPlayField (not (omnipotentEnabled (params state))) state
        c <- getChar
        state' <- turn state c
        ifM (return $ c == 'x') (return state') (gameLoop state')

    builderLoop :: GameState -> IO GameState
    builderLoop state = do
        putStrLn "\ESC[2J"
        putStrLn $ renderPlayField False state
        c <- getChar
        state' <- buildStep state c
        ifM (return $ c == 'x') (return state') (builderLoop state')

    turn :: GameState -> Char -> IO GameState
    turn s c = case c of
        'w' -> return $ updateGameState 0 0 Monster s
        'a' -> return $ updateGameState 0 0 Bomb s
        's' -> return $ updateGameState 0 0 Treasure s
        'd' -> return $ updateGameState 0 0 Empty s
        'x' -> return s
        _ -> return s

    buildStep :: GameState -> Char -> IO GameState
    buildStep = undefined

    updateGameState :: Int -> Int -> Space -> GameState -> GameState
    updateGameState x y val state = GameState { playfield = newPlayfield, cursor = cursor state, params = params state}
        where newPlayfield = take x (playfield state) ++ [updateRow y val (playfield state !! x)] ++ drop (x + 1) (playfield state)

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
    renderRow obs r c = "| " ++ unwords (map (obsfucateSpace obs . show) r) ++ " |"

    instance Show Space where
        show Empty = " ⠀⠀ "
        show You = " 😎 "
        show Monster = " 👹 "
        show Bomb = " 💣 " 
        show Treasure = " 💰 "

    renderSpace :: Cursor -> RenderLocation -> RenderMask -> Space -> String
    renderSpace = undefined

    obsfucateSpace :: Bool -> String -> String
    obsfucateSpace enabled = map (if enabled then obsfucateChar else id)
        where obsfucateChar ch
                | ch == '💣' = '❔'
                | ch == '👹' = '❔'
                | ch == '💰' = '❔'
                | otherwise = ch
