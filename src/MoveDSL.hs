{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module MoveDSL where

import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Reader
import Control.Lens

import GameState
import Data.Array
import Data.Maybe

-- I thought free monads would make the movement code look simpler but idk

data MoveResult = SimpleMove | ChoiceTransform [PType]
type PossibleMove = (BPos,MoveResult)

type Offset = (Int,Int)

data SqVal = SqFull PColor | SqEmpty | SqNone
    deriving (Eq)

isEmpty SqEmpty = True
isEmpty _ = False

isFull (SqFull _) = True
isFull _ = False

-- i'm trying to make the movement code general and independant of piece type
-- so the code can only see colors and relative positions
data MoveDSLF a
    = GetSquare Offset (SqVal -> a)
    | GetLastMove ((BPos,BPos) -> a)
    | GetColor (PColor -> a)
    | GetAbsPos (BPos -> a) -- hmmm idk
    | ReturnMoves [Offset] ([PossibleMove] -> a)
    deriving (Functor)
makeFree ''MoveDSLF
type MoveDSL = Free MoveDSLF

type Movement = MoveDSL [PossibleMove]

-- i don't know what i'm doing here
-- why is the next required?
interpretMoveDSL :: BPos -> MoveDSL a -> MGame a
interpretMoveDSL pos movement = do
    board <- view gsBoard <$> asks state
    let go = \case
            GetSquare offset next -> do
                let npos = pos `addPos` offset
                onboard <- (`onBoard` npos) <$> asks state
                if onboard
                    then pure $ next $ case board ! npos of
                        Just p -> SqFull $ view pColor p
                        Nothing -> SqEmpty
                    else pure $ next SqNone
            GetColor next -> pure $ next $ view pColor $ fromJust $ board ! pos
            ReturnMoves offsets next -> pure $ next $ map ((,SimpleMove) . addPos pos) offsets
            GetLastMove next -> next <$> asks (head . view gsMoves . state)
            GetAbsPos next -> pure $ next pos

    foldFree go movement

canCapture :: (Int,Int) -> MoveDSL Bool
canCapture offset = do
    sq <- getSquare offset
    case sq of
        SqFull c -> (/=c) <$> getColor
        SqEmpty -> pure True
        SqNone -> pure False

pawnMove :: Movement
pawnMove = do
    color <- getColor
    let dir = if color == White
        then 1
        else -1

    sq <- getSquare (0,dir)
    returnMoves $ case sq of
        SqEmpty -> [(0,dir)]
        _ -> []

pawnCapture :: Movement
pawnCapture = do
    color <- getColor
    let dir = if color == White
        then 1
        else -1

    let sqs = [(1,dir),(-1,dir)]
    vals <- traverse getSquare sqs
    returnMoves [fst x | x<- zip sqs vals, snd x == SqFull (oppColor color)]

moveLine :: (Int,Int) -> Movement
moveLine dPos = go dPos [] >>= returnMoves where
    go pos sqs = getSquare pos >>= \case
            SqEmpty -> go (pos `addPos` dPos) (pos: sqs)
            _ -> do
                can <- canCapture pos
                pure $ if can then pos : sqs else sqs

moveSingle :: Offset -> Movement
moveSingle pos = canCapture pos >>= \case
    True -> returnMoves [pos]
    False -> returnMoves []

    -- MoveLine (dx,dy) -> let
    --     line p@(x,y) sqs
    --         | not $ onBoard gs p = sqs
    --         | lpColor == Just color = sqs
    --         | lpColor == Just (oppColor color) = p : sqs
    --         -- | isNothing lpColor = line (x + dx, y + dy) (p : sqs)
    --         | otherwise = line (x + dx, y + dy) (p : sqs)
    --         where lpColor = _pColor <$> (gs ^. gsBoard) ! p
    --     in line (file + dx, rank + dy) []
    --

