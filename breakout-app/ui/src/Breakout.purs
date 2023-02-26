module Breakout
  ( component
  ) where

import Prelude

import CSS (background, block, display, rgb)
import Control.Monad.ST (for)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe as Maybe
import Data.Number (pi)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as EffectRef
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Graphics.Canvas (CanvasElement, Dimensions)
import Graphics.Canvas as Canvas
import Halogen (SubscriptionId)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Halogen.Themes.Bootstrap5 as HB
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.Event.Event as E
import Web.HTML (HTMLElement)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as MET

type ModalDialogInfo
  = { modalId :: String
    , title :: String
    , message :: String
    }

foreign import showModalDialogImpl :: EffectFn1 String Unit

foreign import hideModalDialogImpl :: EffectFn1 String Unit

showModalDialog :: String -> Effect Unit
showModalDialog = runEffectFn1 showModalDialogImpl

hideModalDialog :: String -> Effect Unit
hideModalDialog = runEffectFn1 hideModalDialogImpl

data BrickStatus
  = InActive
  | Active

derive instance genericBrickStatus :: Generic BrickStatus _

derive instance eqBrickStatus :: Eq BrickStatus

instance showBrickStatus :: Show BrickStatus where
  show = genericShow

newtype BricksIndex
  = BricksIndex { row :: Int, column :: Int }

derive newtype instance showBricksIndex :: Show BricksIndex

derive newtype instance eqBricksIndex :: Eq BricksIndex

derive newtype instance ordBricksIndex :: Ord BricksIndex

newtype Brick
  = Brick { x :: Number, y :: Number, status :: BrickStatus }

derive newtype instance showBrick :: Show Brick

type Bricks
  = Map BricksIndex Brick

newtype Score
  = Score Int

derive newtype instance showScore :: Show Score

raiseToScore :: Score -> Score
raiseToScore (Score s) = Score (s + 1)

reachThePassingScore :: Score -> Boolean
reachThePassingScore (Score s) = totalBricks == s

toStringScore :: Score -> String
toStringScore (Score s) = Int.toStringAs Int.decimal s

newtype Lives
  = Lives Int

derive newtype instance showLives :: Show Lives

consumeLives :: Lives -> Maybe Lives
consumeLives = case _ of
  (Lives lives)
    | 1 < lives -> Just $ Lives (lives - 1)
  _ -> Nothing

toStringLives :: Lives -> String
toStringLives (Lives lives) = Int.toStringAs Int.decimal lives

canvasId = "myCanvas" :: String

paddleHeight = 10.0 :: Number

paddleWidth = 75.0 :: Number

ballRadius = 10.0 :: Number

brickRowCount = 3 :: Int

brickColumnCount = 5 :: Int

totalBricks = (brickRowCount * brickColumnCount) :: Int

brickWidth = 75 :: Int

brickHeight = 20 :: Int

brickPadding = 10 :: Int

brickOffsetTop = 30 :: Int

brickOffsetLeft = 30 :: Int

firstScore = Score 0 :: Score

firstLives = Lives 3 :: Lives

type Dataset
  = { x :: Number
    , y :: Number
    , dx :: Number
    , dy :: Number
    , paddleX :: Number
    , leftPressed :: Boolean
    , rightPressed :: Boolean
    , mouseMoved :: Boolean
    , mousePointClientX :: Int
    , bricks :: Bricks
    , score :: Score
    , lives :: Lives
    }

data GameSet
  = OnGame
  | RespawnGame
  | GameOver
  | GameClear

type State
  = { maybeCanvas :: Maybe CanvasElement
    , maybeEmitAnimationFrame :: Maybe SubscriptionId
    , dataset :: Dataset
    }

data Action
  = Initialize
  | Finalize
  | HandleMouseMove MouseEvent
  | HandleAnimationFrame
  | HandleKeyDown KE.KeyboardEvent
  | HandleKeyUp KE.KeyboardEvent
  | HandleDialogCloseButton ModalDialogInfo

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
              , finalize = Just Finalize
              }
    }

initialState :: forall i. i -> State
initialState _ =
  { maybeCanvas: Nothing
  , maybeEmitAnimationFrame: Nothing
  , dataset:
      { x: 0.0
      , y: 0.0
      , dx: 0.0
      , dy: 0.0
      , paddleX: 0.0
      , leftPressed: false
      , rightPressed: false
      , mouseMoved: false
      , mousePointClientX: 0
      , bricks: Map.empty
      , score: firstScore
      , lives: firstLives
      }
  }

gameClearDialog :: ModalDialogInfo
gameClearDialog =
  { modalId: "gameClearDialog"
  , title: "GAME CLEAR"
  , message: "YOU WIN, CONGRATULATIONS!"
  }

gameOverDialog :: ModalDialogInfo
gameOverDialog =
  { modalId: "gameOverDialog"
  , title: "GAME OVER"
  , message: "YOU LOSE"
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.main [ HP.class_ HB.container ]
    [ modalDialog gameClearDialog
    , modalDialog gameOverDialog
    , HH.div [ HP.class_ HB.row ] [ greeting ]
    , HH.div [ HP.class_ HB.row ] [ accordion ]
    , HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
        [ HH.canvas
            [ HP.classes [ HB.m3, HB.border, HB.borderInfo ]
            , style do
                background (rgb 238 238 238)
                display block
            , HP.id canvasId
            , HP.width 480
            , HP.height 320
            ]
        ]
    ]
  where
  greeting =
    HH.div [ HP.class_ HB.col ]
      [ HH.h1 [ HP.classes [ HB.display4, HB.textCenter ] ] [ HH.text "BREAKOUT" ]
      , HH.hr [ HP.class_ HB.my4 ]
      , HH.p [ HP.class_ HB.lead ] [ HH.text "Use Arrow Keys or Mouse to play." ]
      ]

  accordion =
    HH.div
      [ HP.classes [ HB.col, HB.accordion ]
      , HP.id "accordionExample"
      ]
      [ HH.div [ HP.class_ HB.card ] [ headingOne, collapseOne ]
      , HH.div [ HP.class_ HB.card ] []
      ]

  headingOne =
    HH.div
      [ HP.classes [ HB.cardHeader, HB.p0, HB.dGrid, HB.gap0 ]
      , HP.id "headingOne"
      ]
      [ HH.button
          [ HP.classes [ HB.btn, HB.btnOutlineInfo, HB.col12 ]
          , HP.type_ HP.ButtonButton
          , HP.attr (HC.AttrName "data-toggle") "collapse"
          , HP.attr (HC.AttrName "data-target") "#collapseOne"
          , HP.attr (HC.AttrName "area-controls") "collapseOne"
          ]
          [ HH.text "いまの状態" ]
      ]

  collapseOne =
    HH.div
      [ HP.classes [ HB.collapse ]
      , HP.id "collapseOne"
      , HP.attr (HC.AttrName "area-labelledby") "headingOne"
      , HP.attr (HC.AttrName "data-parent") "#accordionExample"
      , HP.attr (HC.AttrName "area-expanded") "false"
      ]
      [ HH.div [ HP.class_ HB.cardBody ] [ HH.text $ show state.dataset ]
      ]

modalDialog :: forall m. ModalDialogInfo -> H.ComponentHTML Action () m
modalDialog input =
  HH.div
    [ HP.classes [ HB.modal, HB.fade, HB.show ]
    , HP.id input.modalId
    , HP.tabIndex (-1)
    , HP.attr (HC.AttrName "role") "dialog"
    , HP.attr (HC.AttrName "data-backdrop") "static"
    , HE.onKeyDown \_ -> (HandleDialogCloseButton input)
    ]
    [ HH.div
        [ HP.classes [ HB.modalDialog, HB.modalDialogCentered ]
        , HP.attr (HC.AttrName "role") "document"
        ]
        [ HH.div [ HP.class_ HB.modalContent ]
            [ HH.div [ HP.class_ HB.modalHeader ]
                [ HH.h5 [ HP.class_ HB.modalTitle ] [ HH.text input.title ]
                , HH.button
                    [ HP.class_ HB.btnClose
                    , HP.type_ HP.ButtonButton
                    , HP.attr (HC.AttrName "data-dismiss") "modal"
                    , HP.attr (HC.AttrName "area-label") "Close"
                    , HE.onClick \_ -> (HandleDialogCloseButton input)
                    ]
                    [ HH.span [ HP.attr (HC.AttrName "area-hidden") "true" ] [ HH.text "×" ] ]
                ]
            , HH.div [ HP.class_ HB.modalBody ] [ HH.p_ [ HH.text input.message ] ]
            , HH.div [ HP.class_ HB.modalFooter ]
                [ HH.button
                    [ HP.classes [ HB.btn, HB.btnPrimary ]
                    , HP.type_ HP.ButtonButton
                    , HP.attr (HC.AttrName "data-dismiss") "modal"
                    , HE.onClick \_ -> (HandleDialogCloseButton input)
                    ]
                    [ HH.text "Close" ]
                ]
            ]
        ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    -- keyboard event
    document <- H.liftEffect $ Window.document =<< HTML.window
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
    -- mouse event
    H.subscribe' \_ ->
      eventListener
        MET.mousemove
        (HTMLDocument.toEventTarget document)
        (map HandleMouseMove <<< ME.fromEvent)
    -- request animation frame
    sid <- H.subscribe $ emitRequestAnimationFrame HandleAnimationFrame
    --
    init <- H.liftEffect initialize
    H.put init { maybeEmitAnimationFrame = Just sid }
  Finalize -> do
    (state :: State) <- H.get
    maybe mempty H.unsubscribe state.maybeEmitAnimationFrame
  HandleAnimationFrame -> do
    (state :: State) <- H.get
    case state.maybeCanvas of
      Just canvas -> do
        ret <- H.liftEffect $ draw canvas state.dataset
        case ret.gameset of
          GameOver -> H.liftEffect $ showModalDialog gameOverDialog.modalId
          GameClear -> H.liftEffect $ showModalDialog gameClearDialog.modalId
          RespawnGame -> do
            respawndData <- H.liftEffect $ respawnDataset canvas ret.nextDataset
            H.modify_ \st -> st { dataset = respawndData }
          OnGame -> H.modify_ \st -> st { dataset = ret.nextDataset }
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
  HandleMouseMove ev ->
    H.modify_ \st ->
      st
        { dataset
          { mouseMoved = true, mousePointClientX = ME.clientX ev }
        }
  HandleDialogCloseButton info -> do
    H.liftEffect $ hideModalDialog info.modalId
    init <- H.liftEffect initialize
    H.put init
  where
  initialize :: Effect State
  initialize =
    let
      st = initialState unit
    in
      Canvas.getCanvasElementById canvasId
        >>= case _ of
            Nothing -> pure st
            Just canvas -> do
              respawnedDataset <- respawnDataset canvas st.dataset
              let
                bricks = coordinateBricks st.dataset.bricks
              pure
                $ st
                    { maybeCanvas = Just canvas
                    , dataset = respawnedDataset { bricks = bricks }
                    }

respawnDataset :: CanvasElement -> Dataset -> Effect Dataset
respawnDataset canvas ds = do
  dim <- Canvas.getCanvasDimensions canvas
  pure
    $ ds
        { x = dim.width / 2.0
        , y = dim.height - 30.0
        , dx = 2.0
        , dy = (-2.0)
        , paddleX = (dim.width - paddleWidth) / 2.0
        }

htmlElementById :: String -> Effect (Maybe HTMLElement)
htmlElementById elementId = do
  document <- Window.document =<< HTML.window
  let
    nonElementParentNode = HTMLDocument.toNonElementParentNode document
  element <- NonElementParentNode.getElementById elementId nonElementParentNode
  pure $ HTMLElement.fromElement =<< element

draw :: CanvasElement -> Dataset -> Effect { nextDataset :: Dataset, gameset :: GameSet }
draw canvas dataset = do
  dim <- H.liftEffect $ Canvas.getCanvasDimensions canvas
  ctx <- Canvas.getContext2D canvas
  --
  Canvas.clearRect ctx { x: 0.0, y: 0.0, width: dim.width, height: dim.height }
  drawBall
  drawPaddle dim
  drawScore dim dataset.score
  drawLives dim dataset.lives
  drawBricks dataset.bricks
  offsetLeft <- canvasElementOffsetLeft canvasId
  pure $ nextDataset dataset dim offsetLeft
  where
  canvasElementOffsetLeft :: String -> Effect Number
  canvasElementOffsetLeft elemId = do
    maybeCanvasElement <- htmlElementById elemId
    maybe (pure 0.0) HTMLElement.offsetLeft $ maybeCanvasElement

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
      , end: pi * 2.0
      , useCounterClockwise: false
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

  drawBricks :: Bricks -> Effect Unit
  drawBricks =
    traverse_
      ( case _ of
          (Brick brick)
            | brick.status == Active -> do
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
          _ -> pure unit
      )

  drawScore :: Dimensions -> Score -> Effect Unit
  drawScore _ score = do
    ctx <- Canvas.getContext2D canvas
    Canvas.setFont ctx "16px Arial"
    Canvas.setFillStyle ctx "#0095DD"
    Canvas.fillText ctx ("Score: " <> toStringScore score) 8.0 20.0

  drawLives :: Dimensions -> Lives -> Effect Unit
  drawLives { width: width } lives = do
    ctx <- Canvas.getContext2D canvas
    Canvas.setFont ctx "16px Arial"
    Canvas.setFillStyle ctx "#0095DD"
    Canvas.fillText ctx ("Lives: " <> toStringLives lives) (width - 65.0) 20.0

coordinateBricks :: Bricks -> Bricks
coordinateBricks bricks =
  ST.run do
    var <- STRef.new bricks
    for 0 brickColumnCount \c ->
      for 0 brickRowCount \r ->
        let
          key = BricksIndex { row: r, column: c }

          value = coord Active key
        in
          STRef.modify (Map.insertWith modify key value) var
    STRef.read var
  where
  modify :: Brick -> Brick -> Brick
  modify (Brick current) (Brick new) = Brick new { status = current.status }

  coord :: BrickStatus -> BricksIndex -> Brick
  coord status (BricksIndex { row: r, column: c }) =
    Brick
      { x: (c * (brickWidth + brickPadding)) + brickOffsetLeft # Int.toNumber
      , y: (r * (brickHeight + brickPadding)) + brickOffsetTop # Int.toNumber
      , status: status
      }

nextDataset :: Dataset -> Dimensions -> Number -> { nextDataset :: Dataset, gameset :: GameSet }
nextDataset dataset@{ x: x, y: y, dx: dx, dy: dy } c canvasElementOffsetLeft =
  let
    { nextGameSet: nextGameSet, nextLives: nextLives } = nextGameSetAndNextLives
  in
    { nextDataset:
        dataset
          { x = x + dx
          , y = y + dy
          , dx = nextDx
          , dy = nextDy
          , paddleX = nextPaddleX
          , mouseMoved = false
          , bricks = nextBricks
          , score = nextScore
          , lives = nextLives
          }
    , gameset: nextGameSet
    }
  where
  maybeCollisionedBrick :: Maybe (Tuple BricksIndex Brick)
  maybeCollisionedBrick = collisionDetection { x: x, y: y } dataset.bricks

  updateBricks :: Tuple BricksIndex Brick -> Map BricksIndex Brick
  updateBricks (Tuple k v) = Map.insert k v dataset.bricks

  nextBricks :: Map BricksIndex Brick
  nextBricks = maybe dataset.bricks updateBricks maybeCollisionedBrick

  hitBrick :: Boolean
  hitBrick = Maybe.isJust maybeCollisionedBrick

  hitCeiling :: Boolean
  hitCeiling = (y + dy) < ballRadius

  hitFloor :: Boolean
  hitFloor = (y + dy) > (c.height - ballRadius)

  hitLeftWall :: Boolean
  hitLeftWall = (x + dx) < ballRadius

  hitRightWall :: Boolean
  hitRightWall = (x + dx) > (c.width - ballRadius)

  hitPaddle :: Boolean
  hitPaddle = hitFloor && between dataset.paddleX (dataset.paddleX + paddleWidth) (x + dx)

  nextScore :: Score
  nextScore = if hitBrick then raiseToScore dataset.score else dataset.score

  nextPaddleX :: Number
  nextPaddleX =
    let
      relativeX = Int.toNumber dataset.mousePointClientX - canvasElementOffsetLeft

      cond =
        { left: dataset.leftPressed
        , right: dataset.rightPressed
        , mouseMoved: dataset.mouseMoved
        }
    in
      clamp 0.0 (c.width - paddleWidth) case cond of
        { left: false, right: true, mouseMoved: _ } -> dataset.paddleX + 7.0
        { left: true, right: false, mouseMoved: _ } -> dataset.paddleX - 7.0
        { left: _, right: _, mouseMoved: true } -> relativeX - paddleWidth / 2.0
        _ -> dataset.paddleX

  nextDx :: Number
  nextDx = if hitLeftWall || hitRightWall then (-dx) else dx

  nextDy :: Number
  nextDy = case true of
    _
      | hitCeiling -> (-dy)
      | hitPaddle -> (-dy)
      | hitBrick -> (-dy)
      | otherwise -> dy

  nextGameSetAndNextLives :: { nextGameSet :: GameSet, nextLives :: Lives }
  nextGameSetAndNextLives = case true of
    _
      | reachThePassingScore nextScore -> { nextGameSet: GameClear, nextLives: dataset.lives }
      | hitFloor && (not hitPaddle) -> case consumeLives dataset.lives of
        Just l -> { nextGameSet: RespawnGame, nextLives: l }
        Nothing -> { nextGameSet: GameOver, nextLives: dataset.lives }
      | otherwise -> { nextGameSet: OnGame, nextLives: dataset.lives }

collisionDetection :: { x :: Number, y :: Number } -> Bricks -> Maybe (Tuple BricksIndex Brick)
collisionDetection ball bricks =
  inactivate
    <$> ( Array.head
          $ Map.toUnfoldableUnordered
          $ Map.filterWithKey collidesBallAndBrick bricks
      )
  where
  inactivate :: Tuple BricksIndex Brick -> Tuple BricksIndex Brick
  inactivate (Tuple idx (Brick brick)) = Tuple idx $ Brick brick { status = InActive }

  collidesBallAndBrick :: BricksIndex -> Brick -> Boolean
  collidesBallAndBrick _ (Brick brick) = case brick.status of
    InActive -> false
    Active
      | between brick.x (brick.x + (Int.toNumber brickWidth)) ball.x
          && between brick.y (brick.y + (Int.toNumber brickHeight)) ball.y -> true
      | otherwise -> false

emitRequestAnimationFrame :: Action -> HS.Emitter Action
emitRequestAnimationFrame val =
  HS.makeEmitter \emitter ->
    H.liftEffect do
      ref <- EffectRef.new Nothing
      let
        loop = do
          emitter val
          animationId <- Window.requestAnimationFrame loop =<< HTML.window
          EffectRef.write (Just animationId) ref
      loop
      pure do
        EffectRef.read ref >>= traverse_ \animationId -> HTML.window >>= Window.cancelAnimationFrame animationId
