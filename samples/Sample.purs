module Sample where

import Prelude

import Effect (Effect)
import Routing.Hash as R
import Sample.Component1 as C1
import Sample.Component2 as C2
import Sample.Component3 as C3
import Sample.Record.Generically as RG
import Sample.Record.Generically as Sample.Record.Generically
import Sample.Record.Manually as Sample.Record.Manually
import VirtualDOM.Impl.Halogen as H

main :: Effect Unit
main = do
  hash <- R.getHash
  case hash of
    "ui1" -> H.uiMountAtId "root" C1.ui
    "ui2" -> H.uiMountAtId "root" C2.ui
    "ui3" -> H.uiMountAtId "root" C3.ui
    "ui-record" -> H.uiMountAtId "root" RG.ui
    _ -> pure unit

-- H.uiMountAtId "root-manual"
--   { view: Sample.Record.Manually.view
--   , init: Sample.Record.Manually.init
--   , update: Sample.Record.Manually.update
--   }
-- H.uiMountAtId "root-generic"
--   { view: Sample.Record.Generically.view
--   , init: Sample.Record.Generically.init
--   , update: Sample.Record.Generically.update
--   }
