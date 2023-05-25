{-
#### The generic way
-}

module Sample.Variant.Generically where

import Prelude

import MVC.Types (UI)
import MVC.Variant.Types (CaseKey(..), VariantMsg, VariantState)
import MVC.Variant.UI (uiVariant)
import Sample.Component1 as C1
import Sample.Component2 as C2
import Sample.Component3 as C3
import Type.Proxy (Proxy(..))
import VirtualDOM as VD

type Msg = VariantMsg
  ( case1 :: Unit
  , case2 :: Unit
  , case3 :: Unit
  )
  ( case1 :: C1.Msg
  , case2 :: C2.Msg
  , case3 :: C3.Msg
  )

type State = VariantState
  ( case1 :: C1.State
  , case2 :: C2.State
  , case3 :: C3.State
  )

ui :: forall html. VD.Html html => UI html Msg State
ui = uiVariant
  { case1: C1.ui
  , case2: C2.ui
  , case3: C3.ui
  }
  { view: { viewUser }
  , initCase: Proxy :: _ "case1"
  }

viewUser :: forall html a. VD.Html html => html a -> (CaseKey -> a) -> Array CaseKey -> html a
viewUser viewCase mka xs =
  VD.div [ VD.id "variant" ]
    [ VD.select [ VD.onChange (CaseKey >>> mka) ]
        ( xs # map \(CaseKey s) ->
            VD.option [ VD.value s ] [ VD.text s ]
        )
    , VD.hr_
    , viewCase
    ]
