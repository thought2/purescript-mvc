module MVC.Variant.Types where

import Prelude

import Data.Newtype (class Newtype)
import Data.Variant (Variant)

newtype VariantState r = VariantState (Variant r)

data VariantMsg rcase rmsg
  = ChildCaseMsg (Variant rmsg)
  | ChangeCase (Variant rcase)
  | ErrMsg String

newtype CaseKey = CaseKey String

-----------------------------------------------------------
--- Instances
-----------------------------------------------------------

derive instance Eq CaseKey

derive instance Newtype CaseKey _

derive instance Newtype (VariantState r) _