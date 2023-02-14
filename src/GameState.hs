{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Control.Lens
import Data.Array
import Data.List
import Control.Monad.Reader

data PType = Pawn | Rook | Bishop | Knight | Queen | King
    deriving (Eq, Ord, Show)

data PColor = White | Black
    deriving (Eq, Show)

oppColor :: PColor -> PColor
oppColor White = Black
oppColor Black = White

data Piece = Piece
    { _pType :: PType
    , _pColor :: PColor
    } deriving (Eq, Show)
makeLenses 'Piece

type Board = Array (Int,Int) (Maybe Piece)
type BPos = (Int,Int)
type Move = (BPos,BPos)

addPos :: BPos -> BPos -> BPos
addPos (a,b) (c,d) = (a + c, b + d)

-- >>> initialBoard ! (4,7)
-- Just (Piece {_pType = King, _pColor = Black})

initialBoard :: Array (Int,Int) (Maybe Piece)
initialBoard = listArray ((0,0),(7,7)) $ concat $ transpose
    [ map (fmap ($ White)) pieces
    , map (fmap ($ White)) pawns
    , empty
    , empty
    , empty
    , empty
    , map (fmap ($ Black)) pawns
    , map (fmap ($ Black)) pieces
    ]
    where
        empty = Nothing <$ [1..8]
        pawns = Just (Piece Pawn) <$ [1..8]
        pieces = map Just [Piece Rook, Piece Knight, Piece Bishop, Piece Queen, Piece King, Piece Bishop, Piece Knight, Piece Rook]

data CMove = CMove
    deriving (Eq, Show)

data GameState = GameState
    { _gsTurn :: PColor
    , _gsBoard :: Board
    , _gsBoardSize :: (Int,Int)
    , _gsMoves :: [Move]
    , _gsUndoableMoves :: Int
    , _gsLastMove :: CMove
    , _gsKingMoved :: (Bool,Bool)
    , _gsKRookMoved :: (Bool,Bool)
    , _gsQRookMoved :: (Bool,Bool)
    } deriving (Eq, Show)

makeLenses 'GameState

initialGameState :: GameState
initialGameState = GameState
    { _gsTurn = White
    , _gsBoard = initialBoard
    , _gsBoardSize = (8,8)
    , _gsMoves = []
    , _gsUndoableMoves = 0
    , _gsLastMove = CMove
    , _gsKingMoved = (False,False)
    , _gsKRookMoved = (False,False)
    , _gsQRookMoved = (False,False)
    }

onBoard :: GameState -> BPos -> Bool
onBoard gs (x,y) = x >= 0 && x < fst (gs ^. gsBoardSize) && y >= 0 && y < snd (gs ^. gsBoardSize)

data GameRules = GameRules

data Game = MkGame
    { rules :: GameRules
    , state :: GameState
    }

type MGame = Reader Game
