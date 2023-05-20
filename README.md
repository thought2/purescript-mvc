# purescript-data-mvc

## Sample
### Imports
For the examples below, we'll need the following imports:


```hs
module Test.ReadmeSample where

import Prelude hiding (div)

import MVC.Record (RecordMsg, RecordState, viewRecord, updateRecord)
import Unsafe.Coerce (unsafeCoerce)
import VirtualDOM as V
```

### Example components
Let's define some example components that we'll use in our app.
The first one could look like this, it's a simple counter:


```hs
data Msg1 = Increment | Decrement

type State1 = Int

update1 :: Msg1 -> State1 -> State1
update1 msg state = case msg of
  Increment -> state + 1
  Decrement -> state - 1

view1 :: forall html. V.Html html => State1 -> html Msg1
view1 state =
  V.div []
    [ V.div [ V.onClick Increment ] [ V.text "more!" ]
    , V.div [ V.onClick Decrement ] [ V.text "less!" ]
    , V.div [] [ V.text ("Count: " <> show state) ]
    ]
```

The next ones we leave unimplemented for now, it's only important that they 
follow the same pattern but have different types:


```hs
data Msg2 = Msg2
data State2 = State2

update2 :: Msg2 -> State2 -> State2
update2 = unsafeCoerce "unimplemented!"

view2 :: forall html. V.Html html => State2 -> html Msg2
view2 = unsafeCoerce "unimplemented!"

--- And one more:

data Msg3 = Msg3
data State3 = State3

update3 :: Msg3 -> State3 -> State3
update3 = unsafeCoerce "unimplemented!"

view3 :: forall html. V.Html html => State3 -> html Msg3
view3 = unsafeCoerce "unimplemented!"
```

### Mount all components, the manual way


```hs
data AppMsg
  = AppMsg1 Msg1
  | AppMsg2 Msg2
  | AppMsg3 Msg3

type AppState =
  { state1 :: State1
  , state2 :: State2
  , state3 :: State3
  }

appUpdate :: AppMsg -> AppState -> AppState
appUpdate msg state = case msg of
  AppMsg1 childMsg -> state
    { state1 = update1 childMsg state.state1 }
  AppMsg2 childMsg -> state
    { state2 = update2 childMsg state.state2 }
  AppMsg3 childMsg -> state
    { state3 = update3 childMsg state.state3 }

appView :: forall html. V.Html html => AppState -> html AppMsg
appView state =
  V.div_
    [ V.div_
        [ V.div_ [ V.text "Field 1:" ]
        , map AppMsg1 $ view1 state.state1
        ]
    , V.div_
        [ V.div_ [ V.text "Field 2:" ]
        , map AppMsg2 $ view2 state.state2
        ]
    , V.div_
        [ V.div_ [ V.text "Field 3:" ]
        , map AppMsg3 $ view3 state.state3
        ]
    ]
```

### Mount all components, generically using this library:


```hs
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

appUpdate' :: Msg' -> State' -> State'
appUpdate' = updateRecord
  { field1: update1
  , field2: update2
  , field3: update3
  }

view' :: forall html. V.Html html => State' -> html Msg'
view' state =
  V.div []
    ( viewRecord
        { field1: view1
        , field2: view2
        , field3: view3
        }
        state
        # map \{ key, viewValue } ->
            V.div_
              [ V.div_ [ V.text ("Field " <> key <> ":") ]
              , viewValue
              ]
    )
```