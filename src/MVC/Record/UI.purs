module MVC.Record.UI where

import MVC.Record (class UpdateRecord, class ViewRecord, updateRecord, viewRecord)
import MVC.Record.Init (class InitRecord, initRecord)
import MVC.Record.Types (RecordMsg, RecordState)
import MVC.Record.View (ViewRecordProps)
import MVC.Types (UI)
import MVC.Util (class MapProp, mapProp)
import Type.Proxy (Proxy(..))

type UIRecordProps :: (Type -> Type) -> Type -> Type -> Type
type UIRecordProps srf msg sta =
  { view :: ViewRecordProps srf msg
  }

class
  UIRecord uis srf rmsg rsta
  | uis -> srf rmsg rsta
  where
  uiRecord
    :: Record uis
    -> UIRecordProps srf (RecordMsg rmsg) (RecordState rsta)
    -> UI srf (RecordMsg rmsg) (RecordState rsta)

instance
  ( MapProp "init" uis inits
  , MapProp "update" uis updates
  , MapProp "view" uis views
  , InitRecord inits rsta
  , UpdateRecord updates rmsg rsta
  , ViewRecord srf views rmsg rsta
  ) =>
  UIRecord uis srf rmsg rsta where
  uiRecord uis props =
    { init: initRecord inits
    , update: updateRecord updates
    , view: viewRecord views props.view
    }
    where
    inits = mapProp prxInit uis
    updates = mapProp prxUpdate uis
    views = mapProp prxView uis

    prxInit = Proxy :: _ "init"
    prxUpdate = Proxy :: _ "update"
    prxView = Proxy :: _ "view"
