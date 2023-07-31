{-
#### Component 3

![UI3](./assets/img/ui3.png)

-}

module Sample.Component3 where

import Prelude

import MVC.Types (UI)
import Chameleon as VD

data Msg = Toggle

type State = Boolean

init :: State
init = false

update :: Msg -> State -> State
update Toggle = not

view :: forall html. VD.Html html => State -> html Msg
view state =
  VD.div [ VD.id "ui3" ]
    [ VD.label_
        [ VD.input
            [ VD.type_ "checkbox"
            , VD.checked state
            , VD.onChange (const Toggle)
            ]
        , VD.text "Checked?"
        ]
    ]

ui :: forall html. VD.Html html => UI html Msg State
ui = { view, update, init }
