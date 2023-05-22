{-
#### Component 2
-}

module Sample.Component2 where

import MVC.Types (UI)
import VirtualDOM as VD

data Msg = SetName String

type State = String

init :: State
init = ""

update :: Msg -> State -> State
update (SetName name) _ = name

view :: forall html. VD.Html html => State -> html Msg
view state =
  VD.div [ VD.id "ui2" ]
    [ VD.div [] [ VD.text "Name:" ]
    , VD.input
        [ VD.type_ "text"
        , VD.onChange SetName
        , VD.value state
        ]
    ]

ui :: forall html. VD.Html html => UI html Msg State
ui = { view, update, init }

{-
![UI2](./assets/gif/ui2.gif)
-}