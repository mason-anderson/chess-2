{-# LANGUAGE DerivingStrategies #-}

module Move where

import Debug.Trace

import Data.Array
import Control.Lens
import qualified Data.Map as M

import GameState
import Data.Maybe
import MoveDSL
import Control.Monad.Reader

pieceMovments = M.fromList
    [ (Pawn,   [pawnMove, pawnCapture])
    , (Rook,   [moveLine (0,1), moveLine (1,0), moveLine (-1,0), moveLine (0,-1)])
    , (Knight, [moveSingle (x,y) | x<-[-2,-1,1,2], y<- [-2,-1,1,2], abs x /= abs y])
    , (Bishop, [moveLine   (x,y) | x<-[-1,1], y<-[-1,1]])
    , (Queen,  [moveLine   (x,y) | x<- [-1,0,1], y<-[-1,0,1], x/= 0 || y/= 0])
    , (King,   [moveSingle (x,y) | x<-[-1,0,1], y<-[-1,0,1], x/= 0 || y /= 0])
    ]

-- convert a movement and a position into a list of possible squares
movementToSquares :: GameState -> BPos -> Movement -> [PossibleMove]
movementToSquares gs pos movement = runReader (interpretMoveDSL pos movement) (MkGame GameRules gs)

-- data Movement = MoveSingle (Int,Int) | MoveLine (Int,Int) | MoveDirectional Int
--
-- pieceMovments = M.fromList
--     [ (Pawn, [MoveDirectional 1])
--     , (Rook, [MoveLine (0,1), MoveLine (1,0), MoveLine (-1,0), MoveLine (0,-1)])
--     , (Bishop, [MoveLine (x,y) | x<-[-1,1], y<-[-1,1]])
--     , (King, [MoveSingle (x,y) | x<-[-1,0,1], y<-[-1,0,1], x/= 0 || y /= 0])
--     , (Queen, [MoveLine (x,y) | x<- [-1,0,1], y<-[-1,0,1], x/= 0 || y/= 0])
--     , (Knight, [MoveSingle (x,y) | x<-[-2,-1,1,2], y<- [-2,-1,1,2], abs x /= abs y])
--     ]

-- convert a movement and a position in a gamestate into a list of possible squares
-- movementToSquares :: GameState -> BPos -> Movement -> [BPos]
-- movementToSquares gs pos@(file,rank) mov = case mov of
--     MoveSingle (dx,dy) -> let
--         dest = (file + dx, rank + dy)
--         destPiece = (gs ^. gsBoard) ! dest
--         in if onBoard gs dest && maybe True ((/=color) . (^. pColor)) destPiece
--             then [dest]
--             else []
--
--     MoveDirectional dy -> let
--         direction = if color == White
--             then 1
--             else -1
--         dest = (file,rank + direction * dy)
--         destPiece = (gs ^. gsBoard) ! dest
--         in [dest | isNothing destPiece]
--
--     MoveLine (dx,dy) -> let
--         line p@(x,y) sqs
--             | not $ onBoard gs p = sqs
--             | lpColor == Just color = sqs
--             | lpColor == Just (oppColor color) = p : sqs
--             -- | isNothing lpColor = line (x + dx, y + dy) (p : sqs)
--             | otherwise = line (x + dx, y + dy) (p : sqs)
--             where lpColor = _pColor <$> (gs ^. gsBoard) ! p
--         in line (file + dx, rank + dy) []
--
--     where
--         piece = fromJust $ (gs ^. gsBoard) ! pos
--         color = piece ^. pColor

-- checks simply if a piece could move somewhere
-- ignores checks, stalemate etc.
isMovePseudoLegal :: GameState -> Move -> Bool
isMovePseudoLegal gs (from,to) = let
    piece = fromJust $ (gs ^. gsBoard) ! from
    -- dest = (gs ^. gsBoard) ! to
    -- destColor = _pColor <$> dest
    pLegalSquares = map fst $ pieceMovments M.! (piece ^. pType) >>= movementToSquares gs from
    in traceShow pLegalSquares $ to `elem` pLegalSquares

isMoveLegal :: GameState -> Move -> Bool
isMoveLegal gs (from,to) = isMovePseudoLegal gs (from,to)

makeMove :: Move -> GameState -> GameState
makeMove (f,t) gs = gs & gsBoard %~ (// [(f,Nothing), (t,fPiece)])
                       & gsTurn %~ oppColor
    where fPiece = (gs ^. gsBoard) ! f

