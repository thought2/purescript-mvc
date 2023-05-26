module Test.MVC.Record.UpdateSpec where

import Prelude

import Data.Variant as V
import MVC.Record (RecordMsg(..), RecordState(..), updateRecord, updateRecordRL)
import Prim.RowList as RL
import Test.MVC.TestTypes (M1, M2(..), M3, S1(..), S2(..), S3(..))
import Type.Function (type (#), type ($))
import Type.Proxy (Proxy(..))

testUpdate :: RecordState (field1 :: S1, field2 :: S2, field3 :: S3)
testUpdate = updateRecord
  { field1: \(_ :: M1) (_ :: S1) -> S1
  , field2: \(_ :: M2) (_ :: S2) -> S2
  , field3: \(_ :: M3) (_ :: S3) -> S3
  }
  (SetField (V.inj (Proxy :: _ "field2") M2))
  ( RecordState
      { field1: S1
      , field2: S2
      , field3: S3
      }
  )

testUpdateRL
  :: RecordState
       ( field1 :: S1
       , field2 :: S2
       , field3 :: S3
       )
testUpdateRL = updateRecordRL
  ( Proxy
      :: _ $ RL.Nil
           # RL.Cons "field3" Unit
           # RL.Cons "field2" Unit
           # RL.Cons "field1" Unit
  )
  { field1: \(_ :: M1) (_ :: S1) -> S1
  , field2: \(_ :: M2) (_ :: S2) -> S2
  , field3: \(_ :: M3) (_ :: S3) -> S3
  }
  ( RecordState
      { field1: S1
      , field2: S2
      , field3: S3
      }
  )
  (V.inj (Proxy :: _ "field2") M2)
