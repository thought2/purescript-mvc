{-
### Mount one component at a time

![UI Variant](./assets/gif/ui-variant.gif)

#### The manual way
-}

module Sample.Variant.Manually where

import Prelude

import MVC.Types (UI)
import Sample.Component1 as C1
import Sample.Component2 as C2
import Sample.Component3 as C3
import VirtualDOM as VD

data Msg
  = ChildMsg ChildMsg
  | ChangeCase Case

data Case = Case1 | Case2 | Case3

data ChildMsg
  = ChildMsg1 C1.Msg
  | ChildMsg2 C2.Msg
  | ChildMsg3 C3.Msg

data State
  = ChildState1 C1.State
  | ChildState2 C2.State
  | ChildState3 C3.State

stateToCase :: State -> Case
stateToCase state = case state of
  ChildState1 _ -> Case1
  ChildState2 _ -> Case2
  ChildState3 _ -> Case3

derive instance Eq State
derive instance Eq Case

init :: State
init = ChildState1 C1.init

update :: Msg -> State -> State
update msg state = case msg of
  ChildMsg childMsg -> case childMsg, state of
    ChildMsg1 childMsg1, ChildState1 childState -> ChildState1 $ C1.update childMsg1 childState
    ChildMsg2 childMsg2, ChildState2 childState -> ChildState2 $ C2.update childMsg2 childState
    ChildMsg3 childMsg3, ChildState3 childState -> ChildState3 $ C3.update childMsg3 childState
    _, _ -> state

  ChangeCase case_ -> case case_ of
    Case1 -> ChildState1 C1.init
    Case2 -> ChildState2 C2.init
    Case3 -> ChildState3 C3.init

view :: forall html. VD.Html html => State -> html Msg
view state =
  VD.div [ VD.id "variant" ]
    [ VD.select [ VD.onChange (handleCase >>> ChangeCase) ]
        [ VD.option [ VD.value "Case1" ] [ VD.text "1" ]
        , VD.option [ VD.value "Case2" ] [ VD.text "2" ]
        , VD.option [ VD.value "Case3" ] [ VD.text "3" ]
        ]
    , VD.hr_
    , map ChildMsg $ viewChild state
    ]
  where
  handleCase = case _ of
    "Case1" -> Case1
    "Case2" -> Case2
    "Case3" -> Case3
    _ -> Case1

viewChild :: forall html. VD.Html html => State -> html ChildMsg
viewChild state = case state of
  ChildState1 childState -> map ChildMsg1 $ C1.view childState
  ChildState2 childState -> map ChildMsg2 $ C2.view childState
  ChildState3 childState -> map ChildMsg3 $ C3.view childState

ui :: forall html. VD.Html html => UI html Msg State
ui = { view, update, init }
