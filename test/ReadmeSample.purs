module Test.ReadmeSample where

import Prelude hiding (div)

data Attr msg = Event String msg
data Html msg
  = Text String
  | Elem String (Array (Attr msg)) (Array (Html msg))

derive instance Functor Html
derive instance Functor Attr

div :: forall msg. Array (Attr msg) -> Array (Html msg) -> Html msg
div = Elem "div"

text :: forall msg. String -> Html msg
text = Text

onClick = Event "onclick"

---

type Msg1 = Int

type State1 = String

view1 :: State1 -> Html Msg1
view1 _ = div [] [ text "Hello!" ]

---

type Msg2 = Number

type State2 = Boolean

view2 :: State2 -> Html Msg2
view2 _ = div [] [ text "Hello!" ]