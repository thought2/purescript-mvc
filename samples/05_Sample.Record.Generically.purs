{-
#### The generic way
-}

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
