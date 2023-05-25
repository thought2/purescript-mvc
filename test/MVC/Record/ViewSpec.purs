module Test.MVC.Record.ViewSpec where

import Prelude

import MVC.Record as ME
import Prim.RowList as RL
import Test.MVC.TestTypes (HTML(..), M1, M2, M3, S1(..), S2(..), S3(..))
import Type.Function (type (#), type ($))
import Type.Proxy (Proxy(..))

testView :: HTML (ME.RecordMsg (field1 :: M1, field2 :: M2, 
 field3 :: M3))
testView = ME.viewRecord
  { field1: \(_ :: S1) -> HTML :: _ M1
  , field2: \(_ :: S2) -> HTML :: _ M2
  , field3: \(_ :: S3) -> HTML :: _ M3
  }
  { viewEntries: \_ -> HTML
  }
  ( ME.RecordState
      { field1: S1
      , field2: S2
      , field3: S3
      }
  )

testViewRL
  :: Array
       ( ME.ViewResult HTML
           ( ME.RecordMsg
               ( field1 :: M1
               , field2 :: M2
               , field3 :: M3
               )
           )
       )
testViewRL = ME.viewRL
  ( Proxy
      :: _ $ RL.Nil
           # RL.Cons "field3" Unit
           # RL.Cons "field2" Unit
           # RL.Cons "field1" Unit
  )
  { field1: \(_ :: S1) -> HTML :: _ M1
  , field2: \(_ :: S2) -> HTML :: _ M2
  , field3: \(_ :: S3) -> HTML :: _ M3
  }
  ( ME.RecordState
      { field1: S1
      , field2: S2
      , field3: S3
      }
  )
