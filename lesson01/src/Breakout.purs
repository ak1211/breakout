module Breakout (component) where

import Prelude
import CSS (background, block, display, margin, marginBottom, marginLeft, marginRight, marginTop, padding, px, rgb)
import CSS.Common (auto)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas (CanvasElement)
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Math as Math

canvasId :: String
canvasId = "myCanvas"

type State
  = {}

data Action
  = Initialize

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

initialState :: forall i. i -> State
initialState _ = {}

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.main
    [ style do
        margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)
        padding (px 0.0) (px 0.0) (px 0.0) (px 0.0)
    ]
    [ HH.canvas
        [ style do
            background (rgb 238 238 238)
            display block
            marginTop (px 0.0)
            marginRight auto
            marginBottom (px 0.0)
            marginLeft auto
        , HP.id canvasId
        , HP.width 480
        , HP.height 320
        ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    maybeCanvas <- H.liftEffect $ Canvas.getCanvasElementById canvasId
    H.liftEffect
      $ case maybeCanvas of
          Nothing -> pure unit
          Just canvas -> draw canvas

draw :: CanvasElement -> Effect Unit
draw canvas = do
  ctx <- Canvas.getContext2D canvas
  --
  Canvas.beginPath ctx
  Canvas.rect ctx
    $ { x: 20.0
      , y: 40.0
      , width: 50.0
      , height: 50.0
      }
  Canvas.setFillStyle ctx "#FF0000"
  Canvas.fill ctx
  Canvas.closePath ctx
  --
  Canvas.beginPath ctx
  Canvas.arc ctx
    $ { x: 240.0
      , y: 160.0
      , radius: 20.0
      , start: 0.0
      , end: Math.pi * 2.0
      }
  Canvas.setFillStyle ctx "green"
  Canvas.fill ctx
  Canvas.closePath ctx
  --
  Canvas.beginPath ctx
  Canvas.rect ctx
    $ { x: 160.0
      , y: 10.0
      , width: 100.0
      , height: 40.0
      }
  Canvas.setStrokeStyle ctx "rgba(0, 0, 255, 0.5)"
  Canvas.stroke ctx
  Canvas.closePath ctx
