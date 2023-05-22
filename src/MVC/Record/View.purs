module MVC.Record.View where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant as V
import MVC.Record.Types (RecordMsg(..), RecordState(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

type ViewResult :: (Type -> Type) -> Type -> Type
type ViewResult html msg = { key :: String, viewValue :: html msg }

class ViewRecord :: (Type -> Type) -> Row Type -> Row Type -> Row Type -> Constraint
class
  ViewRecord html views rmsg rsta

  where
  viewRecord :: Record views -> RecordState rsta -> Array (ViewResult html (RecordMsg rmsg))

instance
  ( RowToList views rl
  , ViewRecordRL html views rl rmsg rsta
  ) =>
  ViewRecord html views rmsg rsta
  where
  viewRecord fieldViews = viewRL prxRL fieldViews
    where
    prxRL = Proxy :: _ rl

---

class ViewRecordRL :: (Type -> Type) -> Row Type -> RowList Type -> Row Type -> Row Type -> Constraint
class
  ViewRecordRL html views rl rmsg rsta
  | rl -> rmsg rsta views
  where
  viewRL
    :: Proxy rl
    -> Record views
    -> (RecordState rsta -> Array (ViewResult html (RecordMsg rmsg)))

instance ViewRecordRL html () RL.Nil () () where
  viewRL _ _ _ = []

instance
  ( Functor html
  , IsSymbol sym
  , ViewRecordRL html views' rl' rmsg' rsta'

  , Row.Cons sym sta rsta' rsta
  , Row.Cons sym msg rmsg' rmsg
  , Row.Cons sym (sta -> html msg) views' views

  , Row.Lacks sym rsta'
  , Row.Lacks sym rmsg'
  , Row.Lacks sym views'

  , Row.Union rmsg' rx rmsg
  ) =>
  ViewRecordRL html views (RL.Cons sym x rl') rmsg rsta
  where
  viewRL _ views (RecordState states) =
    [ head' ] <> tail'
    where
    head :: ViewResult html msg
    head =
      { key: reflectSymbol prxSym
      , viewValue: Record.get prxSym views state
      }

    state :: sta
    state = Record.get prxSym states

    head' :: ViewResult html (RecordMsg rmsg)
    head' =
      { key: head.key
      , viewValue: map (Set <<< V.inj prxSym) head.viewValue
      }

    tail :: Array (ViewResult html (RecordMsg rmsg'))
    tail = viewRL prxRL'
      (Record.delete prxSym views)
      (RecordState (Record.delete prxSym states))

    tail' :: Array (ViewResult html (RecordMsg rmsg))
    tail' = map (\x -> x { viewValue = map (\(Set x') -> Set $ V.expand x') x.viewValue }) tail

    prxSym = Proxy :: _ sym
    prxRL' = Proxy :: _ rl'

