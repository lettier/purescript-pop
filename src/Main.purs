{-
  PureScript Pop
  (C) 2018 David Lettier
  lettier.com
-}

module Main where

import Prelude
  ( class Apply
  , class Eq
  , class Show
  , Unit
  , append
  , apply
  , bind
  , compose
  , const
  , discard
  , flip
  , map
  , not
  , pure
  , show
  , unit
  , void
  , when
  , ($)
  , (&&)
  , (*)
  , (+)
  , (-)
  , (/)
  , (/=)
  , (<)
  , (<=)
  , (==)
  , (>)
  , (>=)
  , (||)
  )
import Foreign (readInt)
import Prim.Row (class Cons, class Lacks, class Union)
import Record as R
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Array (replicate, filter, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.String (joinWith)
import Data.String.CodePoints as SCP
import Data.Map as DM
import Data.Foldable (foldl, for_)
import Data.Set (member, Set)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Control.Alt (alt)
import Control.Monad.Except (runExcept)
import Effect (Effect)
import JQuery as J
import FRP.Behavior (animate, step, unfold, sample_, ABehavior)
import FRP.Behavior.Mouse (position, buttons)
import FRP.Event (Event, fold, create, count, gate)
import FRP.Event.Mouse (getMouse, up, down, withButtons)
import FRP.Event.Time (interval)

data PopMachineState
  = WaitingForCoin
  | ReceivedCoin
  | DispensingPop
  | WaitingForPopRemoval

derive instance genericPopMachineState
  ::  Generic PopMachineState _

instance eqPopMachineState
  ::  Eq PopMachineState
  where
    eq = genericEq

instance showPopMachineState
  ::  Show PopMachineState
  where
    show WaitingForCoin       = "WaitingForCoin"
    show ReceivedCoin         = "ReceivedCoin"
    show DispensingPop        = "DispensingPop"
    show WaitingForPopRemoval = "WaitingForPopRemoval"

main
  ::  Effect Unit
main = do
  J.ready $ do

    { popMachineContainerDiv
    , popMachineDiv
    , functionalPopImg
    , reactivePopImg
    , programmingPopImg
    , popSlotDiv
    , coinSlotDiv
    , statusDisplayDiv
    , popStop1Div
    , popStop2Div
    , popStop3Div
    , coin1Img
    , coin2Img
    , coin3Img
    , popMachineContainerDivBounds
    , popMachineDivBounds
    , coinSlotDivBounds
    , coinBounds
    } <-
      setupPage

    { overCoin
    , isOverCoin
    , lastCoinNumber
    } <-
      setupCoins
        coin1Img
        coin2Img
        coin3Img

    { isOverPopSlot
    } <-
      setupPopSlot
        popSlotDiv

    let mouseOffset =
          { x: popMachineContainerDivBounds.left + popMachineDivBounds.left
          , y: popMachineContainerDivBounds.top  + popMachineDivBounds.top
          }

    mouse <- getMouse

    popMachine <- create

    dispensedFunctionalPop  <- create
    dispensedReactivePop    <- create
    dispensedProgrammingPop <- create

    let dispensingPopEvent =
          gate
            ( fold
                (\ state _ -> state == DispensingPop)
                popMachine.event
                false
            )
            popMachine.event

    let dispensingPopCountEvent = count dispensingPopEvent

    let dispensedPopCount       = step 0     dispensingPopCountEvent
    let functionalPopDispensed  = step false dispensedFunctionalPop.event
    let reactivePopDispensed    = step false dispensedReactivePop.event
    let programmingPopDispensed = step false dispensedProgrammingPop.event

    let mousePosition = position mouse
    let mouseButtons  = buttons  mouse

    let isMouseDown =
          step
            false
            (     (\ {value, buttons} -> value == 1 && member 0 buttons)
            `map` ( withButtons
                      mouse
                      (
                            ((const 0) `map` up)
                      `alt` ((const 1) `map` down)
                      )
                  )
            )

    let isOverCoinSlot =
          step
            false
            ( sample_
                (     (\ mousePosition' ->
                        runWithMousePosition'
                          mousePosition'
                          (\ { x, y } ->
                            pointIntersects'
                              { x: x - mouseOffset.x
                              , y: y - mouseOffset.y
                              }
                              coinSlotDivBounds
                          )
                      )
                `map` mousePosition
                )
                (interval 1)
            )

    let isDragging =
          unfold
            (\
              { isMouseDown'
              , isOverCoin'
              , isOverCoinSlot'
              }
              wasDragging
              ->
                  not isOverCoinSlot'
              &&  (   (wasDragging  && isMouseDown')
                  ||  (isMouseDown' && isOverCoin')
                  )
            )
            ( sample_
                ( combine3Behaviors
                    (Tuple (SProxy :: SProxy "isMouseDown'")    isMouseDown)
                    (Tuple (SProxy :: SProxy "isOverCoin'")     isOverCoin)
                    (Tuple (SProxy :: SProxy "isOverCoinSlot'") isOverCoinSlot)
                )
                (interval 1)
            )
            false

    let popMachineState = step WaitingForCoin popMachine.event

    void $
      animate
        ( combine8Behaviors
            (Tuple (SProxy :: SProxy "lastCoinNumber")    lastCoinNumber)
            (Tuple (SProxy :: SProxy "mousePosition")     mousePosition)
            (Tuple (SProxy :: SProxy "isDragging")        isDragging)
            (Tuple (SProxy :: SProxy "isOverCoinSlot")    isOverCoinSlot)
            (Tuple (SProxy :: SProxy "isOverCoin")        isOverCoin)
            (Tuple (SProxy :: SProxy "mouseButtons")      mouseButtons)
            (Tuple (SProxy :: SProxy "dispensedPopCount") dispensedPopCount)
            (Tuple (SProxy :: SProxy "popMachineState")   popMachineState)
        )
        ( popMachineStateAnimation
            { mouseOffset
            , overCoin
            , popMachine
            , dispensedFunctionalPop
            , dispensedReactivePop
            , dispensedProgrammingPop
            , statusDisplayDiv
            , coinSlotDiv
            , coin1Img
            , coin2Img
            , coin3Img
            , coinBounds
            }
        )

    void $
      animate
        popMachineState
        ( coinCursorAnimation
            [ coin1Img
            , coin2Img
            , coin3Img
            ]
        )

    void $
      animate
        ( combine4Behaviors
            (Tuple (SProxy :: SProxy "mouseButtons")      mouseButtons)
            (Tuple (SProxy :: SProxy "isOverPopSlot")     isOverPopSlot)
            (Tuple (SProxy :: SProxy "dispensedPopCount") dispensedPopCount)
            (Tuple (SProxy :: SProxy "popMachineState")   popMachineState)
        )
        ( popSlotPickUpAnimation
            { popMachine
            , functionalPopImg
            , programmingPopImg
            , reactivePopImg
            }
        )

    void $
      animate
        ( combine3Behaviors
            (Tuple (SProxy :: SProxy "functionalPopDispensed")  functionalPopDispensed)
            (Tuple (SProxy :: SProxy "reactivePopDispensed")    reactivePopDispensed)
            (Tuple (SProxy :: SProxy "programmingPopDispensed") programmingPopDispensed)
        )
        ( popDispenseAnimation
            { popMachine
            , functionalPopImg
            , reactivePopImg
            , programmingPopImg
            , popStop1Div
            , popStop2Div
            , popStop3Div
            }
        )

newtype ID  = ID String
newtype SRC = SRC String

type Bounds =
  {  left    :: Int
  ,  top     :: Int
  ,  right   :: Int
  ,  bottom  :: Int
  ,  width   :: Int
  ,  height  :: Int
  ,  width'  :: Int
  ,  height' :: Int
  }

setupPage
  ::  Effect
        { popMachineContainerDiv       :: J.JQuery
        , popMachineDiv                :: J.JQuery
        , functionalPopImg             :: J.JQuery
        , reactivePopImg               :: J.JQuery
        , programmingPopImg            :: J.JQuery
        , popSlotDiv                   :: J.JQuery
        , coinSlotDiv                  :: J.JQuery
        , statusDisplayDiv             :: J.JQuery
        , popStop1Div                  :: J.JQuery
        , popStop2Div                  :: J.JQuery
        , popStop3Div                  :: J.JQuery
        , coin1Img                     :: J.JQuery
        , coin2Img                     :: J.JQuery
        , coin3Img                     :: J.JQuery
        , popMachineContainerDivBounds :: Bounds
        , popMachineDivBounds          :: Bounds
        , coinSlotDivBounds            :: Bounds
        , coinBounds                   :: Bounds
        }
setupPage = do
  popMachineContainerDiv <- J.select "#popMachineContainer"

  popMachineDiv <- addDiv (ID "popMachine") []

  coinSlotDiv      <- addDiv (ID "coinSlot")      ["floater", "hotSpot"]
  statusDisplayDiv <- addDiv (ID "statusDisplay") ["floater"]
  popSlotDiv       <- addDiv (ID "popSlot")       ["floater", "hotSpot"]
  popStop1Div      <- addDiv (ID "popStop1")      ["floater"]
  popStop2Div      <- addDiv (ID "popStop2")      ["floater"]
  popStop3Div      <- addDiv (ID "popStop3")      ["floater"]

  programmingPopImg <- addImg (ID "programmingPop") (SRC "./images/programming-pop.svg") []
  reactivePopImg    <- addImg (ID "reactivePop")    (SRC "./images/reactive-pop.svg")    []
  functionalPopImg  <- addImg (ID "functionalPop")  (SRC "./images/functional-pop.svg")  []

  popMachineCutoutBackgroundsImg <- addImg (ID "") (SRC "./images/pop-machine-cutout-backgrounds.svg") []
  popMachineImg                  <- addImg (ID "") (SRC "./images/pop-machine.svg")                    []
  popMachineGraphicsImg          <- addImg (ID "") (SRC "./images/pop-machine-graphics.svg")           []
  popMachineWindowImg            <- addImg (ID "") (SRC "./images/pop-machine-window.svg")             []
  coinSlotImg                    <- addImg (ID "") (SRC "./images/coin-slot.svg")                      []
  statusWindowOverlayImg         <- addImg (ID "") (SRC "./images/status-window-overlay.svg")          []

  coin1Img <- addImg  (ID "coinOne")   (SRC "./images/coin.svg") ["floater", "hotSpot", "coin"]
  coin2Img <- addImg  (ID "coinTwo")   (SRC "./images/coin.svg") ["floater", "hotSpot", "coin"]
  coin3Img <- addImg  (ID "coinThree") (SRC "./images/coin.svg") ["floater", "hotSpot", "coin"]

  J.append popMachineDiv popMachineContainerDiv

  J.append popMachineCutoutBackgroundsImg popMachineDiv
  J.append programmingPopImg              popMachineDiv
  J.append reactivePopImg                 popMachineDiv
  J.append functionalPopImg               popMachineDiv
  J.append popMachineImg                  popMachineDiv
  J.append popMachineGraphicsImg          popMachineDiv
  J.append popMachineWindowImg            popMachineDiv
  J.append coinSlotImg                    popMachineDiv

  J.append statusDisplayDiv popMachineDiv

  J.append statusWindowOverlayImg popMachineDiv

  J.append coinSlotDiv popMachineDiv
  J.append popSlotDiv  popMachineDiv
  J.append popStop1Div popMachineDiv
  J.append popStop2Div popMachineDiv
  J.append popStop3Div popMachineDiv

  J.append coin3Img popMachineDiv
  J.append coin2Img popMachineDiv
  J.append coin1Img popMachineDiv

  popMachineContainerDivBounds <- getBounds popMachineContainerDiv

  let statusDisplayDivFontSize = 32
  let statusDisplayDivPadding  = 10

  J.css
    { "font-size": show statusDisplayDivFontSize `append` "px"
    , padding:     show statusDisplayDivPadding  `append` "px"
    }
    statusDisplayDiv

  let baseContainerRefWidth  = 1440
  let baseContainerRefHeight = 1114

  when (popMachineContainerDivBounds.height < baseContainerRefHeight) $ do
    let heightRatio =
            toNumber popMachineContainerDivBounds.height
          / toNumber baseContainerRefHeight

    popMachineDivResizeCss <-
      getResizeCss
        heightRatio
        popMachineDiv
    J.css
      { width: popMachineDivResizeCss.width
      }
      popMachineDiv

    statusDisplayDivResizeCss <-
      getResizeCss
        heightRatio
        statusDisplayDiv
    let statusDisplayDivFontSize' =
                    show (heightRatio * toNumber statusDisplayDivFontSize)
          `append`  "px"
    let statusDisplayDivPadding' =
                    show (heightRatio * toNumber statusDisplayDivPadding)
          `append`  "px"
    J.css
      { width:         statusDisplayDivResizeCss.width
      , height:        statusDisplayDivResizeCss.height
      , top:           statusDisplayDivResizeCss.top
      , left:          statusDisplayDivResizeCss.left
      , "line-height": statusDisplayDivResizeCss.height
      , "font-size":   statusDisplayDivFontSize'
      , padding:       statusDisplayDivPadding'
      }
      statusDisplayDiv

    for_
      [ coinSlotDiv
      , popSlotDiv
      , popStop1Div
      , popStop2Div
      , popStop3Div
      , coin1Img
      , coin2Img
      , coin3Img
      ]
      (resizeElement heightRatio)

    for_
      [ programmingPopImg
      , reactivePopImg
      , functionalPopImg
      , popMachineCutoutBackgroundsImg
      , popMachineImg
      , popMachineGraphicsImg
      , popMachineWindowImg
      , coinSlotImg
      , statusWindowOverlayImg
      ]
      (resizeElementOnLoad heightRatio)

  popMachineDivBounds <- getBounds popMachineDiv
  coinSlotDivBounds   <- getBounds coinSlotDiv
  coinBounds          <- getBounds coin1Img

  pure
    { popMachineContainerDiv
    , popMachineDiv
    , functionalPopImg
    , reactivePopImg
    , programmingPopImg
    , popSlotDiv
    , coinSlotDiv
    , statusDisplayDiv
    , popStop1Div
    , popStop2Div
    , popStop3Div
    , coin1Img
    , coin2Img
    , coin3Img
    , popMachineContainerDivBounds
    , popMachineDivBounds
    , coinSlotDivBounds
    , coinBounds
    }
  where
    addDiv
      ::  ID
      ->  Array String
      ->  Effect J.JQuery
    addDiv (ID id) classes = do
      let attributes =
            DM.insert "class" (joinWith " " classes) $
              DM.insert "id" id $
                DM.empty
      addElement "div" attributes
    addImg
      ::  ID
      ->  SRC
      ->  Array String
      ->  Effect J.JQuery
    addImg (ID id) (SRC src) classes = do
      let attributes =
            DM.insert "alt" "image" $
              DM.insert "class" (joinWith " " classes) $
                DM.insert "onmousedown" "if (event.preventDefault) event.preventDefault()" $
                  DM.insert "src" src $
                    DM.insert "id" id $
                      DM.empty
      addElement "img" attributes
    addElement
      ::  String
      ->  DM.Map String String
      ->  Effect J.JQuery
    addElement elementType attributes = do
      let string =
                      "<"
            `append`  elementType
            `append`  " "
            `append`  flattenAttributes
            `append`  ">"
      J.create string
      where
        flattenAttributes
          ::  String
        flattenAttributes =
          foldl
            (\ s k ->
                        s
              `append`  " "
              `append`  k
              `append`  "='"
              `append`  fromMaybe "" (DM.lookup k attributes)
              `append`  "'"
            )
            ""
            (DM.keys attributes)
    resizeElementOnLoad
      ::  Number
      ->  J.JQuery
      ->  Effect Unit
    resizeElementOnLoad ratio element =
      J.on
        "load"
        (\ _ _ -> resizeElement ratio element)
        element
    resizeElement
      ::  Number
      ->  J.JQuery
      ->  Effect Unit
    resizeElement ratio element = do
      resizeCss <- getResizeCss ratio element
      J.css
        resizeCss
        element
    getResizeCss
      ::  Number
      ->  J.JQuery
      ->  Effect { width :: String, height :: String, top :: String, left :: String }
    getResizeCss ratio element = do
      resizeBounds <- getResizeBounds ratio element
      let show' dimension = show dimension `append` "px"
      pure
        { height: show' resizeBounds.height
        , width:  show' resizeBounds.width
        , top:    show' resizeBounds.top
        , left:   show' resizeBounds.left
        }
    getResizeBounds
      ::  Number
      ->  J.JQuery
      ->  Effect { width :: Number, height :: Number, top :: Number, left :: Number }
    getResizeBounds ratio element = do
      bounds      <- getBounds element
      let height  = if bounds.height > 0 then bounds.height else bounds.height'
      let width   = if bounds.width  > 0 then bounds.width  else bounds.width'
      let height' = toNumber height * ratio
      let width'  = height' * (toNumber width / toNumber height)
      let top     = toNumber bounds.top * ratio
      let left    = top * (toNumber bounds.left / toNumber bounds.top)
      pure
        { height: height'
        , width:  width'
        , top:    top
        , left:   left
        }

setupCoins
  ::  J.JQuery
  ->  J.JQuery
  ->  J.JQuery
  ->  Effect
        { overCoin       :: { event :: Event Int, push :: Int -> Effect Unit }
        , isOverCoin     :: ABehavior Event Boolean
        , lastCoinNumber :: ABehavior Event Int
        }
setupCoins coin1Img coin2Img coin3Img = do
  overCoin <- create
  setupCoin coin1Img overCoin.push 1
  setupCoin coin2Img overCoin.push 2
  setupCoin coin3Img overCoin.push 3
  let isOverCoin =
        step
          false
          ((flip (>) 0) `map` overCoin.event)
  let lastCoinNumber =
        unfold
          (\ c lc ->
            if c > 0
              then c
              else lc
          )
          overCoin.event
          0
  pure
    { overCoin
    , isOverCoin
    , lastCoinNumber
    }
  where
    setupCoin
      ::  J.JQuery
      ->  (Int -> Effect Unit)
      ->  Int
      ->  Effect Unit
    setupCoin coin push int = do
      J.on
        "mouseover"
        (\ _ _ -> push int)
        coin
      J.on
        "mouseout"
        (\ _ _ -> push 0)
        coin

setupPopSlot
  ::  J.JQuery
  ->  Effect
        { overPopSlot   :: { event :: Event Boolean, push :: Boolean -> Effect Unit }
        , isOverPopSlot :: ABehavior Event Boolean
        }
setupPopSlot popSlot = do
  overPopSlot <- create
  J.on
    "mouseover"
    (\ _ _ -> overPopSlot.push true)
    popSlot
  J.on
    "mouseout"
    (\ _ _ -> overPopSlot.push false)
    popSlot

  let isOverPopSlot =
        step
          false
          overPopSlot.event
  pure
    { overPopSlot
    , isOverPopSlot
    }

type DispensedPop = { event :: Event Boolean, push :: Boolean -> Effect Unit }
type PopMachine   = { event :: Event PopMachineState, push :: PopMachineState -> Effect Unit }

popMachineStateAnimation
  ::  { mouseOffset             :: { x :: Int, y :: Int }
      , overCoin                :: { event :: Event Int, push :: Int -> Effect Unit }
      , popMachine              :: PopMachine
      , dispensedFunctionalPop  :: DispensedPop
      , dispensedReactivePop    :: DispensedPop
      , dispensedProgrammingPop :: DispensedPop
      , statusDisplayDiv        :: J.JQuery
      , coinSlotDiv             :: J.JQuery
      , coin1Img                :: J.JQuery
      , coin2Img                :: J.JQuery
      , coin3Img                :: J.JQuery
      , coinBounds              :: Bounds
      }
  ->  { lastCoinNumber    :: Int
      , mousePosition     :: Maybe { x :: Int, y :: Int }
      , isDragging        :: Boolean
      , isOverCoinSlot    :: Boolean
      , isOverCoin        :: Boolean
      , mouseButtons      :: Set Int
      , dispensedPopCount :: Int
      , popMachineState   :: PopMachineState
      }
  ->  Effect Unit
popMachineStateAnimation
  { mouseOffset
  , overCoin
  , popMachine
  , dispensedFunctionalPop
  , dispensedReactivePop
  , dispensedProgrammingPop
  , statusDisplayDiv
  , coinSlotDiv
  , coin1Img
  , coin2Img
  , coin3Img
  , coinBounds
  }
  { lastCoinNumber
  , mousePosition
  , isDragging
  , isOverCoinSlot
  , isOverCoin
  , mouseButtons
  , dispensedPopCount
  , popMachineState
  }
  =
  case popMachineState of
    WaitingForCoin -> do
      setStatusDisplay
        ( if dispensedPopCount >= popCount
            then "No more pop. 😞"
            else "Waiting for a coin."
        )
      if isOverCoinSlot && isOverCoin
        then
          case lastCoinNumber of
            1 -> receiveCoin coin1Img
            2 -> receiveCoin coin2Img
            3 -> receiveCoin coin3Img
            _ -> pure unit
        else
          when isDragging $
            runWithMousePosition
              mousePosition
              (\ { x, y } -> do
                let newCoord x1 x2 d = toNumber x1 - toNumber x2 - (toNumber d / 2.0)
                let x' = newCoord x mouseOffset.x coinBounds.width
                let y' = newCoord y mouseOffset.y coinBounds.height
                let updateCoords =
                      J.css
                        { left: show x' `append` "px"
                        , top:  show y' `append` "px"
                        }
                case lastCoinNumber of
                  1 -> updateCoords coin1Img
                  2 -> updateCoords coin2Img
                  3 -> updateCoords coin3Img
                  _ -> pure unit
              )

    ReceivedCoin -> do
      popMachine.push DispensingPop
      setStatusDisplay ""
      case dispensedPopCount of
        0 -> dispensedProgrammingPop.push true
        1 -> dispensedReactivePop.push    true
        2 -> dispensedFunctionalPop.push  true
        _ -> pure unit

    DispensingPop -> do
      setStatusDisplay "Dispensing you pop..."

    WaitingForPopRemoval ->
      setStatusDisplay "Please take your pop."
  where
    setStatusDisplay
      ::  String
      ->  Effect Unit
    setStatusDisplay message = do
      state <- fromMaybe "" `map` J.getAttr "data-state" statusDisplayDiv
      when (state /= show popMachineState) $ do
        clearStatusDisplay
        J.setAttr
          "data-state"
          (show popMachineState)
          statusDisplayDiv
      html <- J.getHtml statusDisplayDiv
      let message' = message `append` joinWith "" (replicate 50 " ")
      if SCP.length html >= SCP.length message'
        then
          clearStatusDisplay
        else
          J.setHtml
            (html `append` SCP.take 1 (SCP.drop (SCP.length html) message'))
            statusDisplayDiv
      where
        clearStatusDisplay
          ::  Effect Unit
        clearStatusDisplay =
          J.setHtml
            ""
            statusDisplayDiv
    receiveCoin
      ::  J.JQuery
      ->  Effect Unit
    receiveCoin coin = do
      J.hide coin
      popMachine.push ReceivedCoin
      overCoin.push 0

coinCursorAnimation
  ::  Array J.JQuery
  ->  PopMachineState
  ->  Effect Unit
coinCursorAnimation coins popMachineState =
  if popMachineState == WaitingForCoin
    then
      for_
        coins
        (changeCursor "pointer")
    else
      for_
        coins
        (changeCursor "not-allowed")
  where
    changeCursor
      ::  String
      ->  J.JQuery
      ->  Effect Unit
    changeCursor cursor =
      J.css
        { "cursor": cursor }

popSlotPickUpAnimation
  ::  { popMachine        :: PopMachine
      , functionalPopImg  :: J.JQuery
      , programmingPopImg :: J.JQuery
      , reactivePopImg    :: J.JQuery
      }
  ->  { mouseButtons      :: Set Int
      , isOverPopSlot     :: Boolean
      , dispensedPopCount :: Int
      , popMachineState   :: PopMachineState
      }
  ->  Effect Unit
popSlotPickUpAnimation
  { popMachine
  , functionalPopImg
  , programmingPopImg
  , reactivePopImg
  }
  { mouseButtons
  , isOverPopSlot
  , dispensedPopCount
  , popMachineState
  }
  = do
  let leftButton  = member 0 mouseButtons
  let countValid  = dispensedPopCount > 0 && dispensedPopCount <= popCount
  let isDispensed = popMachineState == WaitingForPopRemoval
  let clickedPop  = isOverPopSlot && leftButton && isDispensed && countValid
  when clickedPop $ do
    case dispensedPopCount of
      1 -> J.hide programmingPopImg
      2 -> J.hide reactivePopImg
      3 -> J.hide functionalPopImg
      _ -> pure unit
    popMachine.push WaitingForCoin

popDispenseAnimation
  ::  { functionalPopImg  :: J.JQuery
      , reactivePopImg    :: J.JQuery
      , programmingPopImg :: J.JQuery
      , popStop1Div       :: J.JQuery
      , popStop2Div       :: J.JQuery
      , popStop3Div       :: J.JQuery
      , popMachine        :: PopMachine
      }
  ->  { functionalPopDispensed  :: Boolean
      , reactivePopDispensed    :: Boolean
      , programmingPopDispensed :: Boolean
      }
  ->  Effect Unit
popDispenseAnimation
  { functionalPopImg
  , reactivePopImg
  , programmingPopImg
  , popStop1Div
  , popStop2Div
  , popStop3Div
  , popMachine
  }
  { functionalPopDispensed
  , reactivePopDispensed
  , programmingPopDispensed
  }
  = do
  let count =
        length $
          filter
            ((==) true)
            [ functionalPopDispensed
            , reactivePopDispensed
            , programmingPopDispensed
            ]
  case count of
    1 -> do
      void $    goTo popStop1Div functionalPopImg
      void $    goTo popStop2Div reactivePopImg
      result <- goTo popStop3Div programmingPopImg
      when result $
        popMachine.push WaitingForPopRemoval
    2 -> do
      void $    goTo popStop2Div functionalPopImg
      result <- goTo popStop3Div reactivePopImg
      when result $
        popMachine.push WaitingForPopRemoval
    3 -> do
      result <- goTo popStop3Div functionalPopImg
      when result $
        popMachine.push WaitingForPopRemoval
    _ ->
      pure unit
  where
    goTo
      ::  J.JQuery
      ->  J.JQuery
      ->  Effect Boolean
    goTo stop pop = do
      popBounds  <- getBounds pop
      stopBounds <- getBounds stop
      if popBounds.top < stopBounds.top
        then do
          J.css
            { top: show (popBounds.top + 16) `append` "px"
            }
            pop
          pure false
        else do
          J.css
            { top: show stopBounds.top `append` "px"
            }
            pop
          pure true

pointIntersects
  ::  { x :: Int
      , y :: Int
      }
  ->  J.JQuery
  ->  Effect Boolean
pointIntersects point element = do
  bounds <- getBounds element
  let intersects = pointIntersects' point bounds
  pure intersects

pointIntersects'
  ::  forall e
  .   { x :: Int, y :: Int }
  ->  { left   :: Int
      , right  :: Int
      , top    :: Int
      , bottom :: Int
      | e
      }
  ->  Boolean
pointIntersects' { x, y } bounds = do
      x >= bounds.left
  &&  x <= bounds.right
  &&  y >= bounds.top
  &&  y <= bounds.bottom

getBounds
  ::  J.JQuery
  ->  Effect Bounds
getBounds element = do
  left    <- readInt' "offsetLeft"    element
  top     <- readInt' "offsetTop"     element
  width   <- readInt' "offsetWidth"   element
  height  <- readInt' "offsetHeight"  element
  width'  <- readInt' "naturalWidth"  element
  height' <- readInt' "naturalHeight" element
  pure  { left
        , right: left + width
        , top
        , bottom: top + height
        , width
        , height
        , width'
        , height'
        }
  where
    readInt'
      ::  String
      ->  J.JQuery
      ->  Effect Int
    readInt' s e =
                fromRight'
      `compose` runExcept
      `compose` readInt
      `map`     J.getProp s e
    fromRight'
      ::  forall a
      .   Either a Int
      ->  Int
    fromRight' (Right x) = x
    fromRight' _ = 0

runWithMousePosition
  ::  Maybe ({ x :: Int, y :: Int })
  ->  ({ x :: Int, y :: Int } -> Effect Unit)
  ->  Effect Unit
runWithMousePosition (Just point) f = f point
runWithMousePosition _ f = f { x: 0, y: 0 }

runWithMousePosition'
  ::  forall a
  .   Maybe ({ x :: Int, y :: Int })
  ->  ({ x :: Int, y :: Int } -> a)
  ->  a
runWithMousePosition' (Just point) f = f point
runWithMousePosition' _ f = f { x: 0, y: 0 }

popCount
  ::  Int
popCount = 3

combine3Behaviors
  ::  forall   behavior field1 field2 field3 value1 value2 value3 r1 r2 r3
  .   Apply    behavior
  =>  IsSymbol field1
  =>  Lacks    field1 r2
  =>  Cons     field1 value1 r2 r3
  =>  IsSymbol field2
  =>  Lacks    field2 r1
  =>  Cons     field2 value2 r1 r2
  =>  IsSymbol field3
  =>  Lacks    field3 ()
  =>  Cons     field3 value3 () r1
  =>  Tuple (SProxy field1) (behavior value1)
  ->  Tuple (SProxy field2) (behavior value2)
  ->  Tuple (SProxy field3) (behavior value3)
  ->  behavior { | r3 }
combine3Behaviors
  (Tuple field1 behavior1)
  (Tuple field2 behavior2)
  (Tuple field3 behavior3)
  =
          ( \
            value1
            value2
            value3
            ->
              R.insert field1 value1
                (R.insert field2 value2
                  (R.insert field3 value3 {}
                )
              )
          )
  `map`   behavior1
  `apply` behavior2
  `apply` behavior3

combine4Behaviors
  ::  forall
        behavior
        field1
        field2
        field3
        field4
        value1
        value2
        value3
        value4
        r1
        r2
        r3
        r4
  .   Apply    behavior
  =>  IsSymbol field1
  =>  Lacks    field1 r3
  =>  Cons     field1 value1 r3 r4
  =>  IsSymbol field2
  =>  Lacks    field2 r2
  =>  Cons     field2 value2 r2 r3
  =>  IsSymbol field3
  =>  Lacks    field3 r1
  =>  Cons     field3 value3 r1 r2
  =>  IsSymbol field4
  =>  Lacks    field4 ()
  =>  Cons     field4 value4 () r1
  =>  Tuple (SProxy field1) (behavior value1)
  ->  Tuple (SProxy field2) (behavior value2)
  ->  Tuple (SProxy field3) (behavior value3)
  ->  Tuple (SProxy field4) (behavior value4)
  ->  behavior { | r4 }
combine4Behaviors
  (Tuple field1 behavior1)
  two
  three
  four
  =
          ( \
            value1
            record
            ->
              R.insert field1 value1 record
          )
  `map`   behavior1
  `apply` ( combine3Behaviors
              two
              three
              four
          )

combine8Behaviors
  ::  forall
        behavior
        field1
        field2
        field3
        field4
        field5
        field6
        field7
        field8
        value1
        value2
        value3
        value4
        value5
        value6
        value7
        value8
        r1
        r2
        r3
        r4
        r1'
        r2'
        r3'
        r4'
        r8
  .   Apply    behavior
  =>  Union    r4' r4 r8
  =>  IsSymbol field1
  =>  Lacks    field1 r3'
  =>  Cons     field1 value1 r3' r4'
  =>  IsSymbol field2
  =>  Lacks    field2 r2'
  =>  Cons     field2 value2 r2' r3'
  =>  IsSymbol field3
  =>  Lacks    field3 r1'
  =>  Cons     field3 value3 r1' r2'
  =>  IsSymbol field4
  =>  Lacks    field4 ()
  =>  Cons     field4 value4 () r1'
  =>  IsSymbol field5
  =>  Lacks    field5 r3
  =>  Cons     field5 value5 r3 r4
  =>  IsSymbol field6
  =>  Lacks    field6 r2
  =>  Cons     field6 value6 r2 r3
  =>  IsSymbol field7
  =>  Lacks    field7 r1
  =>  Cons     field7 value7 r1 r2
  =>  IsSymbol field8
  =>  Lacks    field8 ()
  =>  Cons     field8 value8 () r1
  =>  Tuple (SProxy field1) (behavior value1)
  ->  Tuple (SProxy field2) (behavior value2)
  ->  Tuple (SProxy field3) (behavior value3)
  ->  Tuple (SProxy field4) (behavior value4)
  ->  Tuple (SProxy field5) (behavior value5)
  ->  Tuple (SProxy field6) (behavior value6)
  ->  Tuple (SProxy field7) (behavior value7)
  ->  Tuple (SProxy field8) (behavior value8)
  ->  behavior { | r8 }
combine8Behaviors
  one
  two
  three
  four
  five
  six
  seven
  eight
  =
          ( \
            record1
            record2
            -> R.union record1 record2
          )
  `map`   ( combine4Behaviors
              one
              two
              three
              four
          )
  `apply` ( combine4Behaviors
              five
              six
              seven
              eight
          )
