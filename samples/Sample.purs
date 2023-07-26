module Sample where

import Prelude

import Effect (Effect)
import Routing.Hash as R
import Sample.Component1 as C1
import Sample.Component2 as C2
import Sample.Component3 as C3
import Sample.Record.Generically as RG
import Sample.Variant.Generically as VG
import VirtualDOM.Impl.Halogen as H

main :: Effect Unit
main = do
  hash <- R.getHash
  case hash of
    "ui1" -> H.uiMountAtId "root" $ H.uiToHalogenComponent { onStateChange: \_ -> pure unit } C1.ui
    "ui2" -> H.uiMountAtId "root" $ H.uiToHalogenComponent { onStateChange: \_ -> pure unit } C2.ui
    "ui3" -> H.uiMountAtId "root" $ H.uiToHalogenComponent { onStateChange: \_ -> pure unit } C3.ui
    "ui-record" -> H.uiMountAtId "root" $ H.uiToHalogenComponent { onStateChange: \_ -> pure unit } RG.ui
    "ui-variant" -> H.uiMountAtId "root" $ H.uiToHalogenComponent { onStateChange: \_ -> pure unit } VG.ui
    _ -> pure unit
