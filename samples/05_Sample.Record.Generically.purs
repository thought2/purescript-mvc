{-
#### The generic way
-}

module Sample.Record.Generically where

import Prelude

import MVC.Record (RecordMsg, RecordState)
import MVC.Record.UI (uiRecord)
import MVC.Types (UI)
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

ui :: forall html. VD.Html html => UI html Msg State
ui = uiRecord
  { field1: C1.ui
  , field2: C2.ui
  , field3: C3.ui
  }
  { view: { viewEntries } }
---


viewEntries
  :: forall html msg
   . VD.Html html
  => Array { key :: String, viewValue :: html msg }
  -> html msg
viewEntries entries =
  VD.table_
    ( entries # map
        \{ key, viewValue } ->
          VD.tr_
            [ VD.td_ [ VD.text key ]
            , VD.td_ [ viewValue ]
            ]
    )
