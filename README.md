
# purescript-data-mvc

<!-- AUTO-GENERATED-CONTENT:START (TOC) -->
- [Sample](#sample)
  - [Sample Components](#sample-components)
    - [Component 1](#component-1)
    - [Component 2](#component-2)
    - [Component 3](#component-3)
  - [Mount all components](#mount-all-components)
    - [The manual way](#the-manual-way)
    - [The generic way](#the-generic-way)
<!-- AUTO-GENERATED-CONTENT:END -->

## Sample

### Sample Components
To demonstrate the use of this library, we'll first define three simple UI
components. We'll use the
[virtual-dom](https://github.com/thought2/purescript-virtual-dom) library to
define framework agnostic HTML. The components have state which is defined by
simple state update functions.

#### Component 1

<details>
  <summary><code>Sample.Component1</code></summary>


```hs
module Sample.Component1 where

import Prelude
import VirtualDOM as VD
```

</details>


```hs
data Msg = Increment | Decrement

type State = Int

init :: State
init = 0

update :: Msg -> State -> State
update msg state = case msg of
  Increment -> state + 1
  Decrement -> state - 1

view :: forall html. VD.Html html => State -> html Msg
view state =
  VD.div []
    [ VD.button [ VD.onClick Increment ] [ VD.text "more!" ]
    , VD.button [ VD.onClick Decrement ] [ VD.text "less!" ]
    , VD.div [] [ VD.text ("Count: " <> show state) ]
    ]
```
#### Component 2


```hs
module Sample.Component2 where

import Prelude hiding (div)

import VirtualDOM as VD

data Msg = SetName String

type State = String

init :: State
init = ""

update :: Msg -> State -> State
update (SetName name) _ = name

view :: forall html. VD.Html html => State -> html Msg
view state =
  VD.div []
    [ VD.div [] [ VD.text "Name:" ]
    , VD.input
        [ VD.type_ "text"
        , VD.onChange SetName
        , VD.value state
        ]
    ]
```
#### Component 3


```hs
module Sample.Component3 where

import Prelude

import VirtualDOM as VD

data Msg = Toggle

type State = Boolean

init :: State
init = false

update :: Msg -> State -> State
update Toggle = not

view :: forall html. VD.Html html => State -> html Msg
view state =
  VD.div []
    [ VD.label_
        [ VD.input
            [ VD.type_ "checkbox"
            , VD.checked state
            , VD.onChange (const Toggle)
            ]

        , VD.text "Checked?"
        ]
    ]
```
### Mount all components
#### The manual way


```hs
module Sample.Record.Manually where

import Prelude
import Sample.Component1 as C1
import Sample.Component2 as C2
import Sample.Component3 as C3
import VirtualDOM as VD

data Msg
  = Msg1 C1.Msg
  | Msg2 C2.Msg
  | Msg3 C3.Msg

type State =
  { state1 :: C1.State
  , state2 :: C2.State
  , state3 :: C3.State
  }

init :: State
init =
  { state1: C1.init
  , state2: C2.init
  , state3: C3.init
  }

update :: Msg -> State -> State
update msg state = case msg of
  Msg1 childMsg -> state
    { state1 = C1.update childMsg state.state1 }
  Msg2 childMsg -> state
    { state2 = C2.update childMsg state.state2 }
  Msg3 childMsg -> state
    { state3 = C3.update childMsg state.state3 }

view :: forall html. VD.Html html => State -> html Msg
view state =
  VD.table_
    [ VD.tr_
        [ VD.td_ [ VD.text "field1" ]
        , VD.td_ [ map Msg1 $ C1.view state.state1 ]
        ]
    , VD.tr_
        [ VD.td_ [ VD.text "field2" ]
        , VD.td_ [ map Msg2 $ C2.view state.state2 ]
        ]
    , VD.tr_
        [ VD.td_ [ VD.text "field3" ]
        , VD.td_ [ map Msg3 $ C3.view state.state3 ]
        ]
    ]
```
#### The generic way


```hs
module Sample.Record.Generically where

import Prelude

import MVC.Record (RecordMsg, RecordState(..), updateRecord, viewRecord)
import Sample.Component1 as C1
import Sample.Component2 as C2
import Sample.Component3 as C3
import VirtualDOM as VD

type Msg = RecordMsg
  ( field1 :: C1.Msg
  , field2 :: C2.Msg
  , field3 :: C3.Msg
  )

type State = RecordState
  ( field1 :: C1.State
  , field2 :: C2.State
  , field3 :: C3.State
  )

init :: State
init = RecordState
  { field1: C1.init
  , field2: C2.init
  , field3: C3.init
  }

update :: Msg -> State -> State
update = updateRecord
  { field1: C1.update
  , field2: C2.update
  , field3: C3.update
  }

view :: forall html. VD.Html html => State -> html Msg
view state =
  VD.table_
    ( viewRecord
        { field1: C1.view
        , field2: C2.view
        , field3: C3.view
        }
        state
        # map \{ key, viewValue } ->
            VD.tr_
              [ VD.td_ [ VD.text key ]
              , VD.td_ [ viewValue ]
              ]
    )
```
