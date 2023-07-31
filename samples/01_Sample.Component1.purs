{-
# purescript-mvc

MVC types and some relatated generic functions

<!-- AUTO-GENERATED-CONTENT:START (TOC) -->
<!-- AUTO-GENERATED-CONTENT:END -->

## Sample

### Sample Components
To demonstrate the use of this library, we'll first define three simple UI
components. We'll use the
[chameleon](https://github.com/thought2/purescript-chameleon) library to
define framework agnostic HTML. The components have state which is defined by
simple state update functions.

#### Component 1

![UI1](./assets/img/ui1.png)

-}
module Sample.Component1 where

import Prelude

import MVC.Types (UI)
import Chameleon as VD

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
  VD.div [ VD.id "ui1" ]
    [ VD.button [ VD.id "more", VD.onClick Increment ] [ VD.text "more!" ]
    , VD.button [ VD.id "less", VD.onClick Decrement ] [ VD.text "less!" ]
    , VD.div [] [ VD.text ("Count: " <> show state) ]
    ]

ui :: forall html. VD.Html html => UI html Msg State
ui = { view, update, init }