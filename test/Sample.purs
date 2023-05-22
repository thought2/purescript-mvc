module Test.Sample where

import Prelude

import Effect (Effect)
import Test.ReadmeSample as ReadmeSample
import VirtualDOM.Impl.Halogen as H

main :: Effect Unit
main = do
  H.uiMountAtId "root-manual"
    { view: ReadmeSample.appView
    , init: ReadmeSample.appInit
    , update: ReadmeSample.appUpdate
    }
  H.uiMountAtId "root-generic"
    { view: ReadmeSample.appView'
    , init: ReadmeSample.appInit'
    , update: ReadmeSample.appUpdate'
    }
