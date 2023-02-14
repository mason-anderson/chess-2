{-# LANGUAGE TemplateHaskell #-}

module App where

import Data.Text
import Control.Lens

import GameState

data AppModel = AppModel
    { _clickCount :: Int
    , _myText :: Text
    , _gameState :: GameState
    , _moves :: [(Text, Text)]
    } deriving (Eq, Show)

makeLenses 'AppModel

data AppEvent
    = AppInit
    | AppIncrease
    | AppAdd
    | AppMove (BPos,BPos)
    | AppMsgBoard
    | AppGotoPly Int
    deriving (Eq, Show)

