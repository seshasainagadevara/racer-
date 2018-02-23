module Main where

import Data.Maybe
import Data.String
import Data.String
import Prelude
import Types
import UI.Elements
import UI.Events
import UI.Properties
import Math
import Data.Int

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (stack)
import Control.Plus ((<|>))
import DOM.HTML.Event.ErrorEvent (lineNo)
import DOM.HTML.History (back)
import Data.Array (group)
import FRP as F
import FRP.Event as E
import FRP.Event.Keyboard as K
import FRP.Behavior.Keyboard (key)

import FRP.Event.Time
import Halogen.VDom.Types (graft)
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import UI.Util as U
import Data.Number.Format
import Control.Monad.Eff.Ref
foreign import click :: MEvent
foreign import change :: MEvent

-- -- App State
-- type Position = { x :: Int , y :: Int }
-- type Obstacle = { imageUrl :: String , position :: Position }
-- type State = { playerPosition :: Position , obstacles :: Array Obstacle , score :: Int }


widget state = relativeLayout
              [ id_ "1"
              , height "match_parent"
              , width "match_parent"
              , background "#76b852"
              , gravity "center"
              , padding "0,0,0,0"
              , margin "0,0,0,0"

              -- , orientation "vertical"
              ]
              [

                 imageView
                   [
                      height "match_parent"
                     , width "match_parent"
                     , margin "0,0,0,0"
                     , imageUrl "track"

                   ],
                        relativeLayout
                           [ id_ "2"
                           , height "wrap_content"
                           , width "wrap_content"
                           , gravity "center"
                           , margin "900,450,0,0"
                           -- , padding "500,550,100,0"
                           -- , padding "830,420,10,10"

                           ]
                           [
                           imageView
                           [
                              id_ "4"
                            , height "match_parent"
                            , width "match_parent"
                            , imageUrl "divider"
                            , margin state.val


                           ]

                            ],
                relativeLayout
                  [ id_ "3"
                  , height "wrap_content"
                  , width "wrap_content"
                  , gravity "center"

                  -- , padding "440,800,100,0"
                  -- , margin "460,900,0,0"
                  ]
                [

                imageView
                   [  id_ "5" ,
                      height "match_parent"
                     , width "match_parent"
                     , imageUrl "car"

                     , margin state.direction
                     -- , margin state.direction
                   ]


      ],
                   -- ],
                textView
                  [
                      id_ "7"
                    , height "wrap_content"
                    , width "wrap_content"

                    , text "LOGIN"
                    , background state.background
                    , fontStyle "Source Sans Pro-Regular"
                    , gravity "center"
                    , onClick (Some click)

                  ]

             ]


motion::Int -> String
motion val= do
  let k = val + 20

  "0,"<>show k<>",0,0"

dright :: Int -> String
dright val = do
  let v = val + 1

  show v <>"900,0,0"

dleft :: Int -> String
dleft val = do
  let v = val - 1
  show v <> "900,0,0"


main = do
  --- Init State {} empty record--
  U.initializeState
  st <- newRef 0
  --- Update State ----
  state <- U.getState
  _ <- U.updateState "val" 0
  _ <- U.updateState "direction" "460,900,0,0"

  ---- Render Widget ---
  U.render (widget state) listen

  pure unit

-- eval :: forall a b t e. Maybe (EventResp a b) -> Maybe (EventResp a b) -> Eff (console :: CONSOLE | e) (Rec t)


eval _= do
  logShow "coming"
  some <- newRef 0
  state <- U.getState
  if state.val == 0 then U.updateState "val" (motion state.val) else U.updateState "val" 0

whenRight right = do
  logShow("right")
  state <- U.getState
  if state.direction == "460,900,0,0"
       then U.updateState "direction" (dright 460)
   else if state.direction == "800,900,0,0"
        then U.updateState "direction" "800,900,0,0"
     else U.updateState "direction" (dright )


whenLeft left = do
  logShow("left")
  state <- U.getState
  if state.direction == "460,900,0,0"
       then U.updateState "direction" "460,900,0,0"
   else if state.direction == "800,900,0,0"
        then U.updateState "direction" (dleft 800)
     else U.updateState "direction" (dleft )




listen = do

  sig6 <- U.signal "7" "onClick" true

  _ <- sig6.event `E.subscribe` (\_ -> do
                                        log("hello")

                                      )

  let behvRight = whenRight <$> (key 39)
  let behvLeft = whenLeft <$> (key 37)
  let behavior = eval <$> sig6.behavior
  let events = toNumber <$> interval 200
  --
  _ <- U.patch widget behvRight K.down
  _ <- U.patch widget behvLeft K.down



  U.patch widget behavior (events)
