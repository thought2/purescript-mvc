module MVC.Variant.Update where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import MVC.Variant.Types (VariantMsg(..), VariantState(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

class UpdateVariant :: Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class
  UpdateVariant initstates updates rcase rmsg rsta
  where
  updateVariant :: Record initstates -> Record updates -> VariantMsg rcase rmsg -> VariantState rsta -> VariantState rsta

instance
  ( UpdateVariantRL rl updates rmsg rsta
  , RowToList updates rl
  , MatchCase rcase initstates rsta
  ) =>
  UpdateVariant initstates updates rcase rmsg rsta where
  updateVariant initVariantStates updates msg (VariantState vsta) = case msg of
    ChildCaseMsg vmsg ->
      VariantState $ updateRL prxRl updates vmsg vsta
    ChangeCase vcase ->
      VariantState $ matchCase initVariantStates vcase
    ErrMsg _ ->
      VariantState vsta
    where
    prxRl = Proxy :: Proxy rl

---

class UpdateVariantRL :: RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class
  UpdateVariantRL rl updates rmsg rsta
  | rl -> rsta
  where
  updateRL :: Proxy rl -> Record updates -> Variant rmsg -> Variant rsta -> Variant rsta

instance UpdateVariantRL RL.Nil updates rmsg () where
  updateRL _ _ _ = V.case_

instance
  ( Row.Cons sym (msg -> sta -> sta) updatesx updates
  , Row.Cons sym msg rmsgx rmsg
  , Row.Cons sym sta rsta' rsta

  , Row.Lacks sym rsta'
  , Row.Lacks sym updatesx

  , UpdateVariantRL rl' updates rmsg rsta'
  , IsSymbol sym
  , Row.Union rsta' rstax rsta
  , RowToList rsta msgRL
  ) =>
  UpdateVariantRL (RL.Cons sym x rl') updates rmsg rsta
  where
  updateRL _ updates vmsg =
    tail'
      # V.on prxSym
          \sta -> V.inj prxSym $
            ( V.default sta
                # V.on prxSym (\msg -> updateFn msg sta)
            ) vmsg
    where

    tail :: Variant rsta' -> Variant rsta'
    tail = updateRL prxRl' updates vmsg

    tail' :: Variant rsta' -> Variant rsta
    tail' = tail >>> V.expand

    updateFn :: msg -> sta -> sta
    updateFn = Record.get prxSym updates

    prxSym = Proxy :: Proxy sym
    prxRl' = Proxy :: Proxy rl'

---

class MatchCase :: Row Type -> Row Type -> Row Type -> Constraint
class
  MatchCase rcase initstates rsta
  where
  matchCase :: Record initstates -> Variant rcase -> Variant rsta

instance
  ( MatchCaseRL rl rcase initstates rsta
  , RowToList initstates rl
  ) =>
  MatchCase rcase initstates rsta where
  matchCase = matchCaseRL (Proxy :: Proxy rl)

class MatchCaseRL :: RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class
  MatchCaseRL rl rcase initstates rsta
  | rl -> rcase
  where
  matchCaseRL :: Proxy rl -> Record initstates -> Variant rcase -> Variant rsta

instance MatchCaseRL RL.Nil () initstates rsta where
  matchCaseRL _ _ = V.case_

instance
  ( Row.Cons sym sta rstax rsta
  , Row.Cons sym sta initstatesx initstates
  , Row.Cons sym Unit rcase' rcase
  , Row.Lacks sym rcase'
  , MatchCaseRL rl' rcase' initstates rsta
  , IsSymbol sym
  ) =>
  MatchCaseRL (RL.Cons sym x rl') rcase initstates rsta where
  matchCaseRL _ initVariantStates =
    tail
      # V.on prxSym (\_ -> V.inj prxSym $ initVariantState)

    where
    initVariantState :: sta
    initVariantState = Record.get prxSym initVariantStates

    tail :: Variant rcase' -> Variant rsta
    tail = matchCaseRL prxRl' initVariantStates

    prxRl' = Proxy :: Proxy rl'
    prxSym = Proxy :: Proxy sym

