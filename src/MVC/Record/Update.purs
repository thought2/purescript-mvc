module MVC.Record.Update where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import MVC.Record.Types (RecordMsg(..), RecordState(..))

class UpdateRecord rsta rmsg updates where
  updateRecord
    :: Record updates
    -> (RecordMsg rmsg -> RecordState rsta -> RecordState rsta)

instance
  ( RowToList rs rl
  , UpdateRecordRL rl rs rm updates
  ) =>
  UpdateRecord rs rm updates where
  updateRecord updates (Set v) state =
    updateRecordRL (Proxy :: _ rl) updates state v

---

class UpdateRecordRL :: RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class UpdateRecordRL rl rs rm updates | rl -> rs rm updates where
  updateRecordRL
    :: Proxy rl
    -> Record updates
    -> (RecordState rs -> Variant rm -> RecordState rs)

instance UpdateRecordRL RL.Nil rs () () where
  updateRecordRL _ _ _ = V.case_

instance
  ( IsSymbol sym
  , UpdateRecordRL rl' rs rm' updates'
  , Row.Cons sym sta rsx rs
  , Row.Cons sym msg rm' rm
  , Row.Cons sym (msg -> sta -> sta) updates' updates
  , Row.Lacks sym rm'
  , Row.Lacks sym rsx
  , Row.Lacks sym updates'
  ) =>
  UpdateRecordRL (RL.Cons sym x rl') rs rm updates where
  updateRecordRL _ updates (RecordState r) =
    tail
      # V.on prxSym
          ( \msg ->
              let
                updateFn = Record.get prxSym updates
              in
                RecordState $ Record.modify prxSym (updateFn msg) r
          )
    where
    tail :: Variant rm' -> RecordState rs
    tail =
      updateRecordRL prxRL'
        (Record.delete prxSym updates)
        (RecordState r)

    prxSym = Proxy :: _ sym
    prxRL' = Proxy :: _ rl'
