{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Text (Text, pack)
import Monomer
import TextShow
-- import qualified Data.ByteString as B

-- import qualified Monomer.Lens as L

import Data.Default

import BoardWidget
import App
import GameState
import Move
-- import ImageLoader

boardKey :: Text
boardKey = "board"

moveRow = hgrid [ply "e4" 0, ply "e5" 1]
    where ply text num = button text (AppGotoPly num)

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI _wenv model = widgetTree where
    widgetTree = hstack [boardW, vstack [moveList]]
    clickerW = vstack
        [ label "Hello world"
        , spacer
        , hstack
            [ label $ "Click count: " <> showt (model ^. clickCount)
            , spacer
            , button "Increase count" AppAdd
            ]
        ] `styleBasic` [padding 10]
    boardW = nodeKey (boardWidget def AppMove) boardKey
    -- img = imageMem_ (model ^. appImage)
    -- img = image "res/images/PawnWhite.png"
    moveList = vstack [label "Moves", vscroll (vstack $ replicate (length $ model ^. moves) moveRow)]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent _wenv _node model evt = case evt of
    AppInit -> []
    AppIncrease -> [Model (model & clickCount +~ 1)]
    AppAdd -> [Model (model & clickCount .~ 50)]
    AppMove m -> if isMoveLegal (model ^. gameState) m
        then [ Model (model
               & gameState %~ makeMove m
               & moves %~ ((m & each %~ pack . show) :)
               )
             , Event AppMsgBoard]
        else []
    AppMsgBoard -> let key = "board"
                 in [Message key (SetBoard (model ^. gameState . gsTurn) $ model ^. gameState . gsBoard)]
                 -- in [Message key AppAdd]
                 -- in [Message key DoStuff]
    AppGotoPly p -> []

main :: IO ()
main = do
    -- pawn <- readImage "PawnWhite.png"
    let config = [
            appWindowTitle "chess 2",
            appWindowIcon "./assets/images/icon.png",
            appTheme darkTheme,
            appFontDef "Regular" "res/fonts/JetBrainsMono-Regular.ttf",
            appInitEvent AppInit
            ]
    let model = AppModel 0 "" initialGameState []

    startApp model handleEvent buildUI config
