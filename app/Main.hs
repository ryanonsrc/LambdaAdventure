{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Main where
    import System.IO
    import Text.PrettyPrint.Annotated.HughesPJ (renderDecorated, render)
    import Text.Printf (printf)

    import System.Directory.Internal.Prelude (getArgs)
    import Data.Map (argSet)
    import Control.Monad
    import Control.Monad.Extra

    data Space = Empty | You | Monster | Bomb | Treasure
    data Cursor = Location { col :: Int, row :: Int } deriving Eq
    type RenderLocation = Cursor
    type RowLocation = Int
    type ColLocation = Int

    type Row = [Space]
    type PlayField = [Row]
    type Obfuscate = Bool

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
        i <- readInput
        state' <- buildStep state i
        ifM (return $ i == Exit) (return state') (builderLoop state')
        where readInput :: IO Input
              readInput = do
                            input <- getChar
                            case input of
                                '\ESC' -> do
                                    c2 <- getChar
                                    case c2 of
                                        '[' -> do
                                            c3 <- getChar
                                            case c3 of
                                                'A' -> return Up
                                                'B' -> return Down
                                                'C' -> return Right
                                                'D' -> return Left
                                                _   -> return Unknown
                                        _   -> return Unknown
                                'w'      -> return PlaceMonster
                                'a'      -> return PlaceBomb
                                's'      -> return PlaceTreasure
                                'd'      -> return ClearSpace
                                'x'      -> return Exit
                                _        -> return Unknown

    data Input = UpArrow | DownArrow | LeftArrow | RightArrow | PlaceMonster | PlaceTreasure | PlaceBomb | ClearSpace | Exit | Unknown deriving (Eq, Show)

    updateCursor :: GameState -> Input -> IO GameState
    updateCursor gs@(GameState (GameParameters d oe be) pf (Location c r)) dir = case dir of
        UpArrow -> if r > 0 then return $ newGameState (r - 1) c else return gs
        DownArrow -> if r < d then return $ newGameState (r + 1) c else return gs 
        LeftArrow -> if c > 0 then return $ newGameState r (c - 1) else return gs
        RightArrow -> if c < d then return $ newGameState r (c + 1) else return gs
        _ -> return gs
        where newGameState r' c' = GameState {
                                    playfield = pf, 
                                    cursor = Location {col = c', row = r'}, 
                                    params = GameParameters { 
                                        dimension = d, 
                                        omnipotentEnabled = oe, 
                                        builderEnabled = be
                                    }
                                }

    turn :: GameState -> Char -> IO GameState
    turn s c = case c of
        'w' -> return $ updateGameState 0 0 Monster s
        'a' -> return $ updateGameState 0 0 Bomb s
        's' -> return $ updateGameState 0 0 Treasure s
        'd' -> return $ updateGameState 0 0 Empty s
        'x' -> return s
        _ -> return s

    buildStep :: GameState -> Input -> IO GameState
    buildStep gs i = do
                        gs' <- updateCursor gs i
                        case i of
                            ClearSpace -> undefined
                            PlaceBomb -> undefined
                            PlaceMonster -> undefined
                            PlaceTreasure -> undefined
                            _ -> return gs'
                     where updatePlayfield :: GameState -> Space -> GameState
                           updatePlayfield gs@(GameState (GameParameters d oe be) pf (Location c r)) sp = GameState {
                                    playfield = pf, 
                                    cursor = Location {col = c', row = r'}, 
                                    params = GameParameters { 
                                        dimension = d, 
                                        omnipotentEnabled = oe, 
                                        builderEnabled = be
                                    }
                                }

    updateGameState :: Int -> Int -> Space -> GameState -> GameState
    updateGameState x y val state = GameState { playfield = newPlayfield, cursor = cursor state, params = params state}
        where newPlayfield = take x (playfield state) ++ [updateRow y val (playfield state !! x)] ++ drop (x + 1) (playfield state)

    updateRow :: Int -> Space -> Row -> Row
    updateRow y val row =
        take y row ++ [val] ++ drop (y + 1) row

    class (Show b) => RenderMask a b where
        renderVal :: a -> b -> String

    instance RenderMask Obfuscate Space where
        renderVal False s = show s
        renderVal True Monster = "❔"
        renderVal True Bomb = "❔"
        renderVal True Treasure = "❔"
        renderVal True sp = show sp

    instance RenderMask (Cursor, RenderLocation) String where
        renderVal :: (Cursor, RenderLocation) -> String -> String
        renderVal (cur, rloc) s
            | cur == rloc = printf "[%s]" s
            | otherwise = printf " %s " s

    renderSpace :: Obfuscate -> Cursor -> RenderLocation -> Space -> String
    renderSpace obf cur rloc sp = renderVal (cur, rloc) (renderVal obf sp)


    renderPlayField :: Bool -> GameState -> String
    renderPlayField obfuscate state =
        unlines $ zipWith (renderRow obfuscate (cursor state)) [0..] (playfield state)
        

    renderRow :: Obfuscate -> Cursor -> RowLocation -> Row -> String
    renderRow obf c rl r = "| " ++ unwords (zipWith renderSpaceAt [0..] r) ++ " |"
                         where renderSpaceAt :: ColLocation -> Space -> String
                               renderSpaceAt column = renderSpace obf c Location { row = rl, col = column}
    instance Show Space where
        show Empty = " ⠀⠀ "
        show You = " 😎 "
        show Monster = " 👹 "
        show Bomb = " 💣 " 
        show Treasure = " 💰 "