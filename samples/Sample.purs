module Sample where

import Prelude

import Effect (Effect)
import Sample.Record.Manually as Sample.Record.Manually
import Sample.Record.Generically as Sample.Record.Generically 
import VirtualDOM.Impl.Halogen as H

main :: Effect Unit
main = do
  H.uiMountAtId "root-manual"
    { view: Sample.Record.Manually.view
    , init: Sample.Record.Manually.init
    , update: Sample.Record.Manually.update
    }
  H.uiMountAtId "root-generic"
    { view: Sample.Record.Generically.view
    , init: Sample.Record.Generically.init
    , update: Sample.Record.Generically.update
    }
