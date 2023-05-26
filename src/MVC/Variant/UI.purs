module MVC.Variant.UI where

import MVC.Types (UI)
import MVC.Util (class MapProp, mapProp)
import MVC.Variant.Init (class InitVariant, initVariant)
import MVC.Variant.Types (VariantMsg, VariantState)
import MVC.Variant.Update (class UpdateVariant, updateVariant)
import MVC.Variant.View (class ViewVariant, ViewArgs, viewVariant)
import Type.Proxy (Proxy(..))

type UIVariantProps :: forall k. (Type -> Type) -> k -> Type
type UIVariantProps srf initsym =
  { view :: forall msg. ViewArgs srf msg -> srf msg
  , initCase :: Proxy initsym
  }

class UIVariant :: Row Type -> (Type -> Type) -> Symbol -> Row Type -> Row Type -> Row Type -> Constraint
class
  UIVariant uis srf initsym rcase rmsg rsta
  | uis -> srf initsym rmsg rsta
  where
  uiVariant
    :: Record uis
    -> UIVariantProps srf initsym
    -> UI srf (VariantMsg rcase rmsg) (VariantState rsta)

instance
  ( MapProp "init" uis inits
  , MapProp "update" uis updates
  , MapProp "view" uis views
  , InitVariant initsym inits rsta
  , UpdateVariant inits updates rcase rmsg rsta
  , ViewVariant srf views rcase rmsg rsta
  ) =>
  UIVariant uis srf initsym rcase rmsg rsta where
  uiVariant uis props =
    { init
    , update
    , view
    }
    where

    init :: VariantState rsta
    init = initVariant props.initCase inits

    update :: VariantMsg rcase rmsg -> VariantState rsta -> VariantState rsta
    update = updateVariant inits updates

    view :: VariantState rsta -> srf (VariantMsg rcase rmsg)
    view = viewVariant { view: props.view } views

    inits = mapProp prxInit uis
    updates = mapProp prxUpdate uis
    views = mapProp prxView uis

    prxInit = Proxy :: _ "init"
    prxUpdate = Proxy :: _ "update"
    prxView = Proxy :: _ "view"
