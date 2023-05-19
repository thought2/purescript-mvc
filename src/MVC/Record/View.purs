module MVC.Record.View where

import Prelude

import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Function (type ($), type (#))
import Type.Proxy (Proxy(..))

newtype State r = State (Record r)

data Msg r = Set (Variant r)

-----------------------------------------------------------
--- ViewRecord
-----------------------------------------------------------

type Props :: forall k. (k -> Type) -> k -> Type
type Props html msg =
  { viewEntry :: { key :: String, value :: html msg } -> html msg
  }

class ViewRecord :: (Type -> Type) -> Row Type -> Row Type -> Row Type -> Constraint
class
  ViewRecord html views rmsg rsta

  where
  view :: Record views -> State rsta -> Array { key :: String, value :: html (Msg rmsg) }

instance
  ( RowToList views rl
  , ViewRecordRL html views rl rmsg rsta
  ) =>
  ViewRecord html views rmsg rsta
  where
  view fieldViews = viewRL prxRL fieldViews
    where
    prxRL = Proxy :: _ rl

testView
  :: Array
       { key :: String
       , value :: HTML (Msg (field1 :: M1, field2 :: M2, field3 :: M3))
       }
testView = view
  { field1: \(_ :: S1) -> HTML :: _ M1
  , field2: \(_ :: S2) -> HTML :: _ M2
  , field3: \(_ :: S3) -> HTML :: _ M3
  }
  ( State
      { field1: S1
      , field2: S2
      , field3: S3
      }
  )

---

class ViewRecordRL :: (Type -> Type) -> Row Type -> RowList Type -> Row Type -> Row Type -> Constraint
class
  ViewRecordRL html views rl rmsg rsta
  | rl -> rmsg rsta views
  where
  viewRL
    :: Proxy rl
    -> Record views
    -> (State rsta -> Array { key :: String, value :: html (Msg rmsg) })

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
  viewRL _ views (State states) =
    [ head' ] <> tail'
    where
    head :: { key :: String, value :: html msg }
    head =
      { key: reflectSymbol prxSym
      , value: Record.get prxSym views state
      }

    state :: sta
    state = Record.get prxSym states

    head' :: { key :: String, value :: html (Msg rmsg) }
    head' =
      { key: head.key
      , value: map (Set <<< V.inj prxSym) head.value
      }

    tail :: Array { key :: String, value :: html (Msg rmsg') }
    tail = viewRL prxRL'
      (Record.delete prxSym views)
      (State (Record.delete prxSym states))

    tail' :: Array { key :: String, value :: html (Msg rmsg) }
    tail' = map (\x -> x { value = map (\(Set x') -> Set $ V.expand x') x.value }) tail

    prxSym = Proxy :: _ sym
    prxRL' = Proxy :: _ rl'

testViewRL
  :: Array
       { key :: String
       , value :: HTML (Msg (field1 :: M1, field2 :: M2, field3 :: M3))
       }
testViewRL = viewRL
  ( Proxy
      :: _ $ RL.Nil
           # RL.Cons "field3" Unit
           # RL.Cons "field2" Unit
           # RL.Cons "field1" Unit
  )
  { field1: \(_ :: S1) -> HTML :: _ M1
  , field2: \(_ :: S2) -> HTML :: _ M2
  , field3: \(_ :: S3) -> HTML :: _ M3
  }
  ( State
      { field1: S1
      , field2: S2
      , field3: S3
      }
  )

-----------------------------------------------------------
--- Test types
-----------------------------------------------------------

data M1 = M1
data M2 = M2
data M3 = M3

data S1 = S1
data S2 = S2
data S3 = S3

data T1 = T1
data T2 = T2
data T3 = T3

data HTML :: Type -> Type
data HTML a = HTML

-----------------------------------------------------------
--- Instances
-----------------------------------------------------------

derive instance Functor HTML

derive instance Newtype (State r) _