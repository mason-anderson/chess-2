{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module BoardWidget where

import Control.Lens

-- import Data.Sequence
import Data.Typeable
-- import qualified Data.Vector as V
import Data.Array
import Data.Default
import Data.Maybe
import Control.Monad
import qualified Data.ByteString as B
import Data.Text (Text , pack)
-- import Data.Vector.Storable.ByteString
import qualified Data.Map as M
-- import Data.List

import Monomer.Widgets.Single
import qualified Monomer.Lens as L
-- import Widgets.BoxesPalette

import ImageLoader
import GameState

data BoardCfg = BoardCfg
    { _boardSize :: Int
    , _boardColor :: Bool
    , _boardColorScheme :: (Color,Color)
    } deriving (Eq, Show)

instance Default BoardCfg where
    def = BoardCfg
        { _boardSize = 30
        , _boardColor = False
        , _boardColorScheme = (Color 0x8c 0xa2 0xad 0xff, Color 0xff 0xff 0xff 0xff)
        }

data BoardState = BoardState
    { _board :: Board
    , _turn :: PColor
    , _images :: M.Map Text (B.ByteString, Size)
    , _dragged :: Maybe BPos
    , _mousePos :: Point
    , _bottomColor :: PColor
    } deriving (Show)


initialBoardState = BoardState
    { _board = initialBoard
    , _turn = White
    , _images = mempty
    , _dragged = Nothing
    , _mousePos = Point 0 0
    , _bottomColor = White
    }

makeLenses 'BoardState
makeLenses 'BoardCfg

data BoardMsg = SetBoard PColor Board
              | DoStuff
              deriving (Show, Eq, Typeable)

boardWidget cfg event = defaultWidgetNode "boardWidget" widget
    where
        widget = makeBoardWidget cfg initialBoardState event

-- makeBoardWidget :: BoardCfg -> BoardState -> x -> Widget s e
makeBoardWidget :: forall s e. (Typeable e) => BoardCfg -> BoardState -> (Move -> e) -> Widget s e
makeBoardWidget cfg state moveEvent = widget
    where
        squareSize :: Num a => a
        squareSize = fromIntegral $ cfg ^. boardSize

        lightSqColor = fst $ cfg ^. boardColorScheme 
        darkSqColor = snd $ cfg ^. boardColorScheme 

        widget = createSingle state def
            { singleInit = initw
            , singleUseScissor = True
            , singleDrawDecorations = True
            , singleMerge = merge
            , singleHandleEvent = handleEvent
            , singleHandleMessage = handleMessage
            , singleGetSizeReq = getSizeReq
            , singleRender = render
            }

        -- initw :: WidgetEnv s e -> WidgetNode s e -> WidgetResult s e
        initw _wenv node = let
                widgetId = node ^. L.info . L.widgetId
                path = node ^. L.info . L.path
                loadImages = do
                    -- what are you suposed to do here?
                    -- is it even possible to load images into the renderer from here?
                    -- are you just suposed to use addImage every time render is called
                    pure ()

                reqs = [RunTask widgetId path loadImages]
            in resultReqs node reqs

        -- merge :: p -> WidgetNode s1 e1 -> p1 -> BoardState -> WidgetResult s1 e1
        merge _wenv node _oldNode oldState = resultNode newNode where
            newNode = node & L.widget .~ makeBoardWidget cfg oldState moveEvent

        handleEvent _wenv node _target evt = case evt of
            Move p@(Point _ _) -> Just (resultReqs newNode [RenderOnce]) where
                newState = state & mousePos .~ p
                newNode = node & L.widget .~ makeBoardWidget cfg newState moveEvent

            Click (Point x y) _button count -> let
                square = posToSquare (x,y)
                piece =  square >>= ((state ^. board) !)
                pieceColor = _pColor <$> piece

                -- widgetId = node ^. L.info . L.widgetId
                -- path = node ^. L.info . L.path

                nodeNoDragged = node & L.widget .~ makeBoardWidget cfg (state & dragged .~ Nothing) moveEvent
                moveEvt = RaiseEvent $ moveEvent (fromJust (state ^. dragged),fromJust square)
                --
                -- in if
                --     | isJust piece && isNothing (state ^. dragged)
                --         -> let
                --             newstate = state & dragged .~ square
                --             nodedragged = node & l.widget .~ makeboardwidget cfg newstate moveevent
                --             in just (resultreqs nodedragged [])
                --     -- a peice is being dragged
                --     | isJust (state ^. dragged)
                --         -> if isNothing piece
                --             then Just (resultReqs nodeNoDragged [moveEvt])
                --             else Just (resultReqs node [])
                --     | otherwise -> Nothing
                in case state ^. dragged of
                    Just drag -> Just (resultReqs nodeNoDragged [moveEvt])
                    Nothing -> if Just (state ^. turn) == pieceColor
                        -- pick up piece
                        then let
                                newState = state & dragged .~ square
                                nodedragged = node & L.widget .~ makeBoardWidget cfg newState moveEvent
                                in Just (resultReqs nodedragged [])
                        else Nothing



            _ -> Nothing
        getSizeReq _wenv _node = (expandSize 100 1, expandSize 100 1)

        handleMessage wenv node target msg = let
            msg' = cast msg
            -- reqs = [UpdateModel (clickCount .~ 66)]
            reqs = []
            newState = case msg' of
                Just DoStuff -> state & board .~ (Nothing <$ initialBoard)
                Just (SetBoard color b) -> state
                                         & board .~ b
                                         & turn .~ color
                _ -> state
            newNode = node & L.widget .~ makeBoardWidget cfg newState moveEvent
            in Just $ resultReqs newNode reqs

        render wenv _node renderer = do
            drawBoard renderer
            drawPieces renderer

            -- draw dragged peice
            when (isJust $ state ^. dragged) $ do
                let (file,rank) = fromJust $ state ^. dragged
                let Point x y = state ^. mousePos
                drawPiece renderer (fromJust $ (state ^. board) ! (file,rank)) (x - squareSize / 2,y - squareSize / 2)

        posToSquare :: (Double,Double) -> Maybe BPos
        posToSquare (x,y)
            | (file >= 0 && file < 8) && (rank >= 0 && rank < 8) = Just (file,rank)
            | otherwise = Nothing
            where
            (file,rank) = (x / squareSize, y / squareSize)&both %~ floor

        drawBoard :: Renderer -> IO ()
        drawBoard renderer = do
            let squarePositions = [(file,rank)  | file<-[0..7::Int], rank<-[0..7]]
            let drawSquare (xi,yi) file rank = drawRect renderer (Rect x y squareSize squareSize) color Nothing
                    where
                        x = squareSize * fromIntegral file + xi
                        y = squareSize * fromIntegral rank + yi
                        color = Just $ if even (file + rank) then lightSqColor else darkSqColor

            beginPath renderer

            -- draw pieces
            forM_ squarePositions $ \sq -> do
                uncurry ( drawSquare (0,0) ) sq


        -- TODO make compile time
        pieceToImg :: Piece -> Text
        pieceToImg (Piece p c) = pack $ show p ++ show c ++ ".png"

        drawPiece :: Renderer -> Piece -> (Double,Double) -> IO ()
        drawPiece renderer piece (x,y) = do
            beginPath renderer
            setFillImagePattern renderer (pieceToImg piece) (Point x y) (Size 30 30) 0 1
            drawRoundedRect renderer (Rect x y 30 30) (Radius Nothing Nothing Nothing Nothing)
            fill renderer

        drawPieces :: Renderer -> IO ()
        drawPieces renderer = do
            loadPieces renderer
            let drawPiece' = drawPiece renderer

            -- pawn <- getImage renderer "PawnWhite.png"
            -- when (isNothing pawn) $
                -- putStrLn "ok"

            -- drawPiece' (Piece Pawn White) (1,1)
            forM_ [(a,b) | a<-[0..7], b<-[0..7]] $ \ pos@(file,rank) -> do
                let (x,y) = (fromIntegral file * squareSize, fromIntegral rank * squareSize)
                let piece = (state ^. board) ! pos
                when (isJust piece && Just pos /= (state ^. dragged)) $ do
                    drawPiece' (fromJust piece) (x,y)

            -- beginPath renderer
            -- setFillImagePattern renderer "PawnWhite.png" (Point 0 0) (Size 30 30) 0 1
            -- drawRoundedRect renderer (Rect 0 0 30 30) (Radius Nothing Nothing Nothing Nothing)
            -- fill renderer
