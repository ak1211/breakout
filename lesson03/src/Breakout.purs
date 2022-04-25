module Breakout (component) where

import Prelude
import CSS (background, block, display, margin, marginBottom, marginLeft, marginRight, marginTop, padding, px, rgb)
import CSS.Common (auto)
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas (CanvasElement, Dimensions)
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Math as Math

canvasId :: String
canvasId = "myCanvas"

type Dataset
  = { x :: Number
    , y :: Number
    , dx :: Number
    , dy :: Number
    , ballRadius :: Number
    }

type State
  = Maybe
      { canvas :: CanvasElement
      , dataset :: Dataset
      }

data Action
  = Initialize
  | Tick

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
initialState _ = Nothing

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
    case maybeCanvas of
      Nothing -> H.put Nothing
      Just canvas -> do
        dim <- H.liftEffect $ Canvas.getCanvasDimensions canvas
        H.put $ initialize canvas dim
        _ <- H.subscribe =<< timer Tick
        pure unit
  Tick ->
    H.get
      >>= maybe mempty \state -> do
          dim <- H.liftEffect $ Canvas.getCanvasDimensions state.canvas
          H.liftEffect $ draw state.canvas dim state.dataset
          H.put $ Just state { dataset = nextDataset dim state.dataset }
  where
  initialize :: CanvasElement -> Dimensions -> State
  initialize canvas dim =
    Just
      { canvas: canvas
      , dataset:
          { x: dim.width / 2.0
          , y: dim.height - 30.0
          , dx: 2.0
          , dy: (-2.0)
          , ballRadius: 10.0
          }
      }

drawBall :: CanvasElement -> Dataset -> Effect Unit
drawBall canvas ds = do
  ctx <- Canvas.getContext2D canvas
  --
  Canvas.beginPath ctx
  Canvas.arc ctx
    { x: ds.x
    , y: ds.y
    , radius: ds.ballRadius
    , start: 0.0
    , end: Math.pi * 2.0
    }
  Canvas.setFillStyle ctx "#0095DD"
  Canvas.fill ctx
  Canvas.closePath ctx

draw :: CanvasElement -> Dimensions -> Dataset -> Effect Unit
draw canvas dim ds = do
  ctx <- Canvas.getContext2D canvas
  --
  Canvas.clearRect ctx { x: 0.0, y: 0.0, width: dim.width, height: dim.height }
  drawBall canvas ds

nextDataset :: Dimensions -> Dataset -> Dataset
nextDataset { width: width, height: height } dataset@{ x: x
, y: y
, dx: dx
, dy: dy
, ballRadius: r
} =
  dataset
    { x = x + dx
    , y = y + dy
    , dx =
      if between (0.0 + r) (width - r) (x + dx) then
        dx
      else
        (-dx)
    , dy =
      if between (0.0 + r) (height - r) (y + dy) then
        dy
      else
        (-dy)
    }

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <-
    H.liftAff $ Aff.forkAff
      $ forever do
          Aff.delay $ Milliseconds 10.0
          H.liftEffect $ HS.notify listener val
  pure emitter
