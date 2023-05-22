module MVC.Util where

import Data.Symbol (class IsSymbol)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))

class MapProp :: Symbol -> Row Type -> Row Type -> Constraint
class MapProp sym ri ro | sym ri -> ro where
  mapProp :: Proxy sym -> { | ri } -> { | ro }

instance
  ( HMap (FnMapProp sym) (Record ri) (Record ro)
  ) =>
  MapProp sym ri ro
  where
  mapProp sym = hmap (FnMapProp sym)

data FnMapProp (k :: Symbol) = FnMapProp (Proxy k)

instance (IsSymbol k, Row.Cons k a rx r) => Mapping (FnMapProp k) { | r } a where
  mapping (FnMapProp k) = Record.get k

testMapProp :: { field1 :: Int, field2 :: Int, field3 :: Int }
testMapProp = mapProp (Proxy :: _ "a")
  { field1: { a: 1 }
  , field2: { a: 2 }
  , field3: { a: 3 }
  }
