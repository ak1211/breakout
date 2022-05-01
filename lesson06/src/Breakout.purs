module Breakout (component) where

import Prelude
import CSS (background, block, display, margin, marginBottom, marginLeft, marginRight, marginTop, padding, px, rgb)
import CSS.Common (auto)
import Control.Monad.Rec.Class (forever)
import Control.Monad.ST (for)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas (CanvasElement, Dimensions)
import Graphics.Canvas as Canvas
import Halogen (SubscriptionId)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Math as Math
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

canvasId :: String
canvasId = "myCanvas"

paddleHeight = 10.0 :: Number

paddleWidth = 75.0 :: Number

ballRadius = 10.0 :: Number

brickRowCount = 3 :: Int

brickColumnCount = 5 :: Int

brickWidth = 75 :: Int

brickHeight = 20 :: Int

brickPadding = 10 :: Int

brickOffsetTop = 30 :: Int

brickOffsetLeft = 30 :: Int

newtype BricksIndex
  = BricksIndex { row :: Int, column :: Int }

derive newtype instance showBricksIndex :: Show BricksIndex

derive newtype instance ordBricksIndex :: Ord BricksIndex

derive newtype instance eqBricksIndex :: Eq BricksIndex

newtype Brick
  = Brick { x :: Number, y :: Number }

derive newtype instance showBrick :: Show Brick

type Dataset
  = { x :: Number
    , y :: Number
    , dx :: Number
    , dy :: Number
    , paddleX :: Number
    , leftPressed :: Boolean
    , rightPressed :: Boolean
    , bricks :: Map BricksIndex Brick
    }

type State
  = { maybeCanvas :: Maybe CanvasElement
    , maybeTimer :: Maybe SubscriptionId
    , dataset :: Dataset
    }

data Action
  = Initialize
  | Tick
  | HandleKeyDown KE.KeyboardEvent
  | HandleKeyUp KE.KeyboardEvent

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
initialState _ =
  { maybeCanvas: Nothing
  , maybeTimer: Nothing
  , dataset:
      { x: 0.0
      , y: 0.0
      , dx: 0.0
      , dy: 0.0
      , paddleX: 0.0
      , leftPressed: false
      , rightPressed: false
      , bricks: Map.empty
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
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
    , HH.text $ show state.dataset
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    -- keyboard event
    document <- H.liftEffect $ Window.document =<< window
    H.subscribe' \_ ->
      eventListener
        KET.keydown
        (HTMLDocument.toEventTarget document)
        (map HandleKeyDown <<< KE.fromEvent)
    H.subscribe' \_ ->
      eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map HandleKeyUp <<< KE.fromEvent)
    -- timer
    sid <- H.subscribe =<< timer Tick
    --
    init <- H.liftEffect initialize
    H.put init { maybeTimer = Just sid }
  Tick -> do
    (state :: State) <- H.get
    case { a: state.maybeCanvas, b: state.maybeTimer } of
      { a: Just canvas, b: Just timerSubscriptionId } -> do
        ret <- H.liftEffect $ draw canvas state.dataset
        if ret.gameover then do
          H.liftEffect $ Window.alert "GAME OVER" =<< window
          H.liftEffect $ Location.reload =<< Window.location =<< window
          H.unsubscribe timerSubscriptionId
        else
          H.modify_ \st -> st { dataset = ret.nextDataset }
      _ -> pure unit
  HandleKeyDown ev
    | KE.key ev == "Right" || KE.key ev == "ArrowRight" -> do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      H.modify_ \st -> st { dataset { rightPressed = true } }
    | KE.key ev == "Left" || KE.key ev == "ArrowLeft" -> do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      H.modify_ \st -> st { dataset { leftPressed = true } }
    | otherwise -> pure unit
  HandleKeyUp ev
    | KE.key ev == "Right" || KE.key ev == "ArrowRight" -> do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      H.modify_ \st -> st { dataset { rightPressed = false } }
    | KE.key ev == "Left" || KE.key ev == "ArrowLeft" -> do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      H.modify_ \st -> st { dataset { leftPressed = false } }
    | otherwise -> pure unit
  where
  initialize :: Effect State
  initialize =
    Canvas.getCanvasElementById canvasId
      >>= case _ of
          Nothing -> pure $ initialState unit
          Just canvas -> do
            dim <- Canvas.getCanvasDimensions canvas
            pure
              { maybeCanvas: Just canvas
              , maybeTimer: Nothing
              , dataset:
                  { x: dim.width / 2.0
                  , y: dim.height - 30.0
                  , dx: 2.0
                  , dy: (-2.0)
                  , paddleX: (dim.width - paddleWidth) / 2.0
                  , leftPressed: false
                  , rightPressed: false
                  , bricks: Map.empty
                  }
              }

draw :: CanvasElement -> Dataset -> Effect { nextDataset :: Dataset, gameover :: Boolean }
draw canvas dataset = do
  dim <- H.liftEffect $ Canvas.getCanvasDimensions canvas
  ctx <- Canvas.getContext2D canvas
  --
  Canvas.clearRect ctx { x: 0.0, y: 0.0, width: dim.width, height: dim.height }
  drawBall
  drawPaddle dim
  bricks <- pure $ coordinateBricks dataset.bricks
  drawBricks bricks
  pure $ nextDataset dataset { bricks = bricks } dim
  where
  drawBall :: Effect Unit
  drawBall = do
    ctx <- Canvas.getContext2D canvas
    --
    Canvas.beginPath ctx
    Canvas.arc ctx
      { x: dataset.x
      , y: dataset.y
      , radius: ballRadius
      , start: 0.0
      , end: Math.pi * 2.0
      }
    Canvas.setFillStyle ctx "#0095DD"
    Canvas.fill ctx
    Canvas.closePath ctx

  drawPaddle :: Dimensions -> Effect Unit
  drawPaddle { height: height } = do
    ctx <- Canvas.getContext2D canvas
    --
    Canvas.beginPath ctx
    Canvas.rect ctx
      { x: dataset.paddleX
      , y: height - paddleHeight
      , width: paddleWidth
      , height: paddleHeight
      }
    Canvas.setFillStyle ctx "#0095DD"
    Canvas.fill ctx
    Canvas.closePath ctx

  drawBricks :: Map BricksIndex Brick -> Effect Unit
  drawBricks bricks =
    for_ bricks \(Brick brick) -> do
      ctx <- Canvas.getContext2D canvas
      Canvas.beginPath ctx
      Canvas.rect ctx
        { x: brick.x
        , y: brick.y
        , width: Int.toNumber brickWidth
        , height: Int.toNumber brickHeight
        }
      Canvas.setFillStyle ctx "#0095DD"
      Canvas.fill ctx
      Canvas.closePath ctx

  coordinateBricks :: Map BricksIndex Brick -> Map BricksIndex Brick
  coordinateBricks bricks =
    ST.run do
      var <- STRef.new bricks
      for 0 brickColumnCount \c ->
        for 0 brickRowCount \r ->
          let
            key = BricksIndex { row: r, column: c }

            value = Brick (coord key)
          in
            STRef.modify (Map.insert key value) var
      STRef.read var
    where
    coord :: BricksIndex -> { x :: Number, y :: Number }
    coord (BricksIndex { row: r, column: c }) =
      { x: (c * (brickWidth + brickPadding)) + brickOffsetLeft # Int.toNumber
      , y: (r * (brickHeight + brickPadding)) + brickOffsetTop # Int.toNumber
      }

nextDataset :: Dataset -> Dimensions -> { nextDataset :: Dataset, gameover :: Boolean }
nextDataset dataset c =
  let
    { x: x, y: y, dx: dx, dy: dy } = dataset

    r = ballRadius

    newPaddleX = case { left: dataset.leftPressed, right: dataset.rightPressed } of
      { left: false, right: true } -> dataset.paddleX + 7.0
      { left: true, right: false } -> dataset.paddleX - 7.0
      _ -> dataset.paddleX

    { nextDy: nextDy, gameover: gameover } =
      let
        touchTheCeiling = (y + dy) < r

        touchTheFloor = (y + dy) > (c.height - r)

        touchThePaddle = touchTheFloor && between dataset.paddleX (dataset.paddleX + paddleWidth) (x + dx)
      in
        case true of
          _
            | touchTheCeiling -> { nextDy: (-dy), gameover: false }
            | touchThePaddle -> { nextDy: (-dy), gameover: false }
            | touchTheFloor -> { nextDy: (-dy), gameover: true }
            | otherwise -> { nextDy: dy, gameover: false }
  in
    { nextDataset:
        dataset
          { x = x + dx
          , y = y + dy
          , dx =
            if between r (c.width - r) (x + dx) then
              dx
            else
              (-dx)
          , dy = nextDy
          , paddleX = clamp 0.0 (c.width - paddleWidth) newPaddleX
          }
    , gameover: gameover
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
