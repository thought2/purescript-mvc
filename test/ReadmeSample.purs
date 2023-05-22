{-
# purescript-data-mvc

<!-- AUTO-GENERATED-CONTENT:START (TOC) -->
toc will be generated here
<!-- AUTO-GENERATED-CONTENT:END -->

## Sample
### Imports
For the examples below, we'll need the following imports:
-}

module Test.ReadmeSample where

import Prelude hiding (div)

import Data.Int as Int
import Data.Maybe (fromMaybe)
import MVC.Record (RecordMsg, RecordState(..), updateRecord, viewRecord)
import VirtualDOM as VD

{-
### Example components
Let's define some example components that we'll use in our app.

#### Component 1
-}

data Msg1 = Increment | Decrement

type State1 = Int

init1 :: State1
init1 = 0

update1 :: Msg1 -> State1 -> State1
update1 msg state = case msg of
  Increment -> state + 1
  Decrement -> state - 1

view1 :: forall html. VD.Html html => State1 -> html Msg1
view1 state =
  VD.div []
    [ VD.button [ VD.onClick Increment ] [ VD.text "more!" ]
    , VD.button [ VD.onClick Decrement ] [ VD.text "less!" ]
    , VD.div [] [ VD.text ("Count: " <> show state) ]
    ]

{-
#### Component 2
-}

data Msg2 = SetName String

type State2 = String

init2 :: State2
init2 = ""

update2 :: Msg2 -> State2 -> State2
update2 (SetName name) _ = name

view2 :: forall html. VD.Html html => State2 -> html Msg2
view2 state =
  VD.div []
    [ VD.div [] [ VD.text "Name:" ]
    , VD.input
        [ VD.type_ "text"
        , VD.onChange SetName
        , VD.value state
        ]
    ]

{-
#### Component 3
-}

data Msg3 = SetAge Int

type State3 = Int

init3 :: State3
init3 = 0

update3 :: Msg3 -> State3 -> State3
update3 (SetAge age) _ = age

view3 :: forall html. VD.Html html => State3 -> html Msg3
view3 state =
  VD.div []
    [ VD.div [] [ VD.text "Age:" ]
    , VD.input
        [ VD.type_ "text"
        , VD.onChange $ strToInt >>> SetAge
        , VD.value (show state)
        ]
    ]
  where
  strToInt = Int.fromStringAs Int.decimal >>> fromMaybe 0

{-
### Mount all components
#### The manual way
-}

data AppMsg
  = AppMsg1 Msg1
  | AppMsg2 Msg2
  | AppMsg3 Msg3

type AppState =
  { state1 :: State1
  , state2 :: State2
  , state3 :: State3
  }

appInit :: AppState
appInit =
  { state1: init1
  , state2: init2
  , state3: init3
  }

appUpdate :: AppMsg -> AppState -> AppState
appUpdate msg state = case msg of
  AppMsg1 childMsg -> state
    { state1 = update1 childMsg state.state1 }
  AppMsg2 childMsg -> state
    { state2 = update2 childMsg state.state2 }
  AppMsg3 childMsg -> state
    { state3 = update3 childMsg state.state3 }

appView :: forall html. VD.Html html => AppState -> html AppMsg
appView state =
  VD.table_
    [ VD.tr_
        [ VD.td_ [ VD.text "field1" ]
        , VD.td_ [ map AppMsg1 $ view1 state.state1 ]
        ]
    , VD.tr_
        [ VD.td_ [ VD.text "field2" ]
        , VD.td_ [ map AppMsg2 $ view2 state.state2 ]
        ]
    , VD.tr_
        [ VD.td_ [ VD.text "field3" ]
        , VD.td_ [ map AppMsg3 $ view3 state.state3 ]
        ]
    ]

{-
#### The generic way using this library
-}

type Msg' = RecordMsg
  ( field1 :: Msg1
  , field2 :: Msg2
  , field3 :: Msg3
  )

type State' = RecordState
  ( field1 :: State1
  , field2 :: State2
  , field3 :: State3
  )

appInit' :: State'
appInit' = RecordState
  { field1: init1
  , field2: init2
  , field3: init3
  }

appUpdate' :: Msg' -> State' -> State'
appUpdate' = updateRecord
  { field1: update1
  , field2: update2
  , field3: update3
  }

appView' :: forall html. VD.Html html => State' -> html Msg'
appView' state =
  VD.table_
    ( viewRecord
        { field1: view1
        , field2: view2
        , field3: view3
        }
        state
        # map \{ key, viewValue } ->
            VD.tr_
              [ VD.td_ [ VD.text key ]
              , VD.td_ [ viewValue ]
              ]
    )

{-
### Mount one of the components
#### The manual way
-}