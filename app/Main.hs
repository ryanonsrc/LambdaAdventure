{-# LANGUAGE MultiParamTypeClasses #-}

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

    data Space = Empty | You | Monster | Bomb | Treasure | Brick
    data Cursor = Location { col :: Int, row :: Int } deriving Eq

    data TurnCursor = UL | UM | UR | ML | MR | LL | LM | LR

    turnCursor :: TurnCursor -> Cursor -> Cursor
    turnCursor UL you = Location { col = col you -1, row = row you -1}
    turnCursor UM you = Location { col = col you,    row = row you -1}
    turnCursor UR you = Location { col = col you +1, row = row you -1}
    turnCursor ML you = Location { col = col you -1, row = row you   }
    turnCursor MR you = Location { col = col you +1, row = row you   }
    turnCursor LL you = Location { col = col you -1, row = row you +1}
    turnCursor LM you = Location { col = col you   , row = row you +1}
    turnCursor LR you = Location { col = col you +1, row = row you +1}


    findYou :: PlayField -> Maybe Cursor
    findYou pf = case [(c, r) | r <- [0..length pf -1], c <- [0..length (pf !! r) - 1], pf !! r !! c == You] of
        [] -> Nothing
        (x:_) -> Just Location {col = fst x, row = scd x}

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
        buildCursor :: Cursor,
        turnCursorPosition :: TurnCursor,
        messageBoard :: String,
        gameOver :: Bool
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
        let state = GameState { params = p, playfield = emptyPlayField (dimension p), buildCursor = Location {col = 0, row = 0}, turnCursorPosition = UL, messageBoard = "", gameOver = False }
        state' <- ifM (return $ builderEnabled (params state)) (builderLoop state) (return state)
        _ <- gameLoop state'
        return ()

    emptyPlayField :: Int -> PlayField
    emptyPlayField n = replicate n (replicate n Empty)


    gameLoop :: GameState -> IO GameState
    gameLoop state = do
        putStrLn "\ESC[2J"
        state' <- setTurnCursor state
        putStrLn $ renderPlayField (not (omnipotentEnabled (params state))) state
        state' <- yourTurn state
        state'' <- theirTurn state'
        ifM (return $ gameOver state'') (return state'') (gameLoop state'')

    setTurnCursor :: GameState -> IO GameState
    setTurnCursor gs = undefined

    yourTurn :: GameState -> IO GameState
    yourTurn gs = do
        i <- readInput
        join updateTurnCursor gs i
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
                                                'A' -> return UpArrow
                                                'B' -> return DownArrow
                                                'C' -> return RightArrow
                                                'D' -> return LeftArrow
                                                _   -> return Unknown
                                        _   -> return Unknown
                                'a'      -> return PlaceBomb
                                'q'      -> return PlaceBrick
                                'e'      -> return MoveHere
                                'x'      -> return Exit
                                _        -> return Unknown



    updateTurnCursor :: GameState -> Input -> IO GameState
    updateTurnCursor = undefined

    theirTurn :: GameState -> IO GameState
    theirTurn = undefined


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
                                                'A' -> return UpArrow
                                                'B' -> return DownArrow
                                                'C' -> return RightArrow
                                                'D' -> return LeftArrow
                                                _   -> return Unknown
                                        _   -> return Unknown
                                'w'      -> return PlaceMonster
                                'a'      -> return PlaceBomb
                                's'      -> return PlaceTreasure
                                'd'      -> return ClearSpace
                                'q'      -> return PlaceBrick
                                'x'      -> return Exit
                                _        -> return Unknown

    data Input = UpArrow | DownArrow | LeftArrow | RightArrow | PlaceMonster | PlaceTreasure | PlaceBomb | PlaceBrick | ClearSpace | MoveHere | Exit | Unknown deriving (Eq, Show)

    endsTurn :: Input -> Bool
    endsTurn MoveHere = True
    endsTurn Exit = True
    endsTurn _ = False

    updateBuildCursor :: GameState -> Input -> IO GameState
    updateBuildCursor gs@(GameState (GameParameters d oe be) pf (Location c r) _ mb gmo) dir = case dir of
        UpArrow -> if r > 0 then return $ newGameState (r - 1) c else return gs
        DownArrow -> if r < d - 1 then return $ newGameState (r + 1) c else return gs
        LeftArrow -> if c > 0 then return $ newGameState r (c - 1) else return gs
        RightArrow -> if c < d - 1 then return $ newGameState r (c + 1) else return gs
        _ -> return gs
        where newGameState r' c' = GameState {
                                    playfield = pf,
                                    buildCursor = Location {col = c', row = r'},
                                    params = GameParameters {
                                        dimension = d,
                                        omnipotentEnabled = oe,
                                        builderEnabled = be
                                    },
                                    turnCursorPosition = UL,
                                    messageBoard = mb,
                                    gameOver = gmo
                                }

    buildStep :: GameState -> Input -> IO GameState
    buildStep gs i = do
                        gs' <- updateBuildCursor gs i
                        case i of
                            ClearSpace -> return $ updatePlayfield gs' Empty
                            PlaceBomb -> return $ updatePlayfield gs' Bomb
                            PlaceMonster -> return $ updatePlayfield gs' Monster
                            PlaceTreasure -> return $ updatePlayfield gs' Treasure
                            PlaceBrick -> return $ updatePlayfield gs' Brick
                            _ -> return gs'
                     where updatePlayfield :: GameState -> Space -> GameState
                           updatePlayfield (GameState (GameParameters d oe be) pf c _ mb gmo) sp = GameState {
                                    playfield = placeOnPlayfield c sp pf,
                                    buildCursor = c,
                                    params = GameParameters {
                                        dimension = d,
                                        omnipotentEnabled = oe,
                                        builderEnabled = be
                                    },
                                    turnCursorPosition = UL,
                                    messageBoard = mb,
                                    gameOver = gmo
                                }

    placeOnPlayfield :: Cursor -> Space -> PlayField -> PlayField
    placeOnPlayfield (Location c r) sp pf = take r pf ++ [updateRow c sp (pf !! r)] ++ drop (r + 1) pf

    updateRow :: Int -> Space -> Row -> Row
    updateRow y val r = take y r ++ [val] ++ drop (y + 1) r

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
        show Brick = " 🧱 "