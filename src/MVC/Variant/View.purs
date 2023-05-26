module MVC.Variant.View
  ( ViewArgs
  , ViewVariantProps
  , caseKeyToVariant
  , caseKeyToVariantRL
  , class CaseKeyToVariant
  , class CaseKeyToVariantRL
  , class GetKeys
  , class GetSym
  , class GetSymRL
  , class ViewVariant
  , class ViewVariantRL
  , getKeys
  , getSym
  , getSymRL
  , viewVariant
  , viewVariantRL
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import MVC.Variant.Types (CaseKey(..), VariantMsg(..), VariantState(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

type ViewArgs src msg =
  { viewCase :: src msg
  , mkMsg :: CaseKey -> msg
  , caseKey :: CaseKey
  , caseKeys :: Array CaseKey
  }

type ViewVariantProps html =
  { view :: forall msg. ViewArgs html msg -> html msg
  }

class ViewVariant :: (Type -> Type) -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class
  ViewVariant html views rcase rmsg rsta
  where
  viewVariant :: ViewVariantProps html -> Record views -> VariantState rsta -> html (VariantMsg rcase rmsg)

instance
  ( ViewVariantRL rl html views rmsg rsta
  , RowToList views rl
  , CaseKeyToVariant rcase
  , Functor html
  , GetKeys rl
  , GetSym rsta
  ) =>
  ViewVariant html views rcase rmsg rsta where
  viewVariant d views (VariantState rsta) = viewUser'
    where
    viewCases :: html (Variant rmsg)
    viewCases = viewVariantRL prxRl views rsta

    viewCases' :: html (VariantMsg rcase rmsg)
    viewCases' = viewCases # map ChildCaseMsg

    caseKeyToVariantMsg :: CaseKey -> VariantMsg rcase rmsg
    caseKeyToVariantMsg = caseKeyToVariant >>> case _ of
      Nothing -> ErrMsg "Invalid case provided by user view"
      Just vcase -> ChangeCase vcase

    viewUser' :: html (VariantMsg rcase rmsg)
    viewUser' = d.view
      { viewCase: viewCases'
      , mkMsg: caseKeyToVariantMsg
      , caseKey: CaseKey $ getSym rsta
      , caseKeys: CaseKey <$> getKeys prxRl
      }

    prxRl = Proxy :: Proxy rl

---

class CaseKeyToVariant rcase where
  caseKeyToVariant :: CaseKey -> Maybe (Variant rcase)

instance
  ( CaseKeyToVariantRL rl rcase
  , RowToList rcase rl
  ) =>
  CaseKeyToVariant rcase where
  caseKeyToVariant = caseKeyToVariantRL (Proxy :: Proxy rl)

class CaseKeyToVariantRL :: RowList Type -> Row Type -> Constraint
class
  CaseKeyToVariantRL rl rcase
  | rl -> rcase
  where
  caseKeyToVariantRL :: Proxy rl -> CaseKey -> Maybe (Variant rcase)

instance CaseKeyToVariantRL RL.Nil () where
  caseKeyToVariantRL _ _ = Nothing

instance
  ( CaseKeyToVariantRL rl' rcase'
  , Row.Cons sym Unit rcase' rcase
  , Row.Lacks sym rcase'
  , IsSymbol sym
  , Row.Union rcase' rcasex rcase
  ) =>
  CaseKeyToVariantRL (RL.Cons sym x rl') rcase where
  caseKeyToVariantRL _ givenKey =
    if caseKey == givenKey then
      head
    else
      tail'

    where
    caseKey = CaseKey $ reflectSymbol prxSym

    head :: Maybe (Variant rcase)
    head = Just $ V.inj prxSym unit

    tail :: Maybe (Variant rcase')
    tail = caseKeyToVariantRL prxRl' givenKey

    tail' :: Maybe (Variant rcase)
    tail' = tail # map V.expand

    prxSym = Proxy :: Proxy sym
    prxRl' = Proxy :: Proxy rl'

---
class ViewVariantRL :: RowList Type -> (Type -> Type) -> Row Type -> Row Type -> Row Type -> Constraint
class
  ViewVariantRL rl html views rmsg rsta
  | rl -> rmsg rsta
  where
  viewVariantRL :: Proxy rl -> Record views -> Variant rsta -> html (Variant rmsg)

instance ViewVariantRL RL.Nil html views () () where
  viewVariantRL _ _ = V.case_

instance
  ( ViewVariantRL rl' html views rmsg' rsta'
  , Row.Cons sym msg rmsg' rmsg
  , Row.Cons sym sta rsta' rsta
  , Row.Cons sym (sta -> html msg) viewsx views
  , Row.Lacks sym rmsg'
  , Row.Lacks sym rsta'
  , Functor html
  , IsSymbol sym
  , Row.Union rmsg' rmsgx rmsg
  ) =>
  ViewVariantRL (RL.Cons sym x rl') html views rmsg rsta
  where
  viewVariantRL _ views =
    tail'
      # V.on prxSym viewFn'

    where
    tail :: Variant rsta' -> html (Variant rmsg')
    tail = viewVariantRL prxRl' views

    tail' :: Variant rsta' -> html (Variant rmsg)
    tail' = tail >>> map V.expand

    viewFn :: sta -> html msg
    viewFn = Record.get prxSym views

    viewFn' :: sta -> html (Variant rmsg)
    viewFn' = viewFn >>> map (V.inj prxSym)

    prxSym = Proxy :: Proxy sym
    prxRl' = Proxy :: Proxy rl'

--------------------------------------------------------------------------------
-- Keys
--------------------------------------------------------------------------------

class GetKeys :: RowList Type -> Constraint
class GetKeys rl where
  getKeys :: Proxy rl -> Array String

instance GetKeys RL.Nil where
  getKeys _ = []

instance (IsSymbol sym, GetKeys rl') => GetKeys (RL.Cons sym a rl') where
  getKeys _ = [ head ] <> tail
    where
    head = reflectSymbol prxSym
    tail = getKeys prxRl'
    prxSym = Proxy :: Proxy sym
    prxRl' = Proxy :: Proxy rl'

--------------------------------------------------------------------------------
-- GetSym
--------------------------------------------------------------------------------

class GetSym r where
  getSym :: Variant r -> String

instance
  ( RowToList r rl
  , GetSymRL rl r
  ) =>
  GetSym r where
  getSym = getSymRL (Proxy :: Proxy rl)

---
class GetSymRL :: forall k. k -> Row Type -> Constraint
class GetSymRL rl r | rl -> r where
  getSymRL :: Proxy rl -> Variant r -> String

instance GetSymRL RL.Nil () where
  getSymRL _ = V.case_

instance
  ( GetSymRL rl' r'
  , Row.Cons sym a r' r
  , IsSymbol sym
  , Row.Lacks sym r'
  , Row.Union r' rx r
  ) =>
  GetSymRL (RL.Cons sym a rl') r
  where
  getSymRL _ =
    getSymRL prxRl'
      # V.on prxSym getSym'

    where
    getSym' :: a -> String
    getSym' _ = reflectSymbol prxSym

    prxSym = Proxy :: Proxy sym
    prxRl' = Proxy :: Proxy rl'
