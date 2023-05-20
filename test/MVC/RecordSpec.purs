module Test.MVC.RecordSpec where

import Prelude

import MVC.Record as ME
import Prim.RowList as RL
import Type.Function (type ($), type (#))
import Type.Proxy (Proxy(..))

-----------------------------------------------------------
--- View
-----------------------------------------------------------

testView :: Array (ME.ViewResult HTML (ME.RecordMsg (field1 :: M1, field2 :: M2, field3 :: M3)))
testView = ME.viewRecord
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

testViewRL :: Array (ME.ViewResult HTML (ME.RecordMsg (field1 :: M1, field2 :: M2, field3 :: M3)))
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


-----------------------------------------------------------
--- Update
-----------------------------------------------------------

testUpdate :: State (field1 :: S1, field2 :: S2, field3 :: S3)
testUpdate = update
  { field1: \(_ :: M1) (_ :: S1) -> S1
  , field2: \(_ :: M2) (_ :: S2) -> S2
  , field3: \(_ :: M3) (_ :: S3) -> S3
  }
  (Set (V.inj (Proxy :: _ "field2") M2))
  ( State
      { field1: S1
      , field2: S2
      , field3: S3
      }
  )


testUpdateRL :: State (field1 :: S1, field2 :: S2, field3 :: S3)
testUpdateRL = updateRL
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
  ( State
      { field1: S1
      , field2: S2
      , field3: S3
      }
  )
  (V.inj (Proxy :: _ "field2") M2)

-----------------------------------------------------------
--- Test types
-----------------------------------------------------------

data M1 = M1
data M2 = M2
data M3 = M3

data S1 = S1
data S2 = S2
data S3 = S3

data T1 = T1
data T2 = T2
data T3 = T3

data HTML :: Type -> Type
data HTML a = HTML

-----------------------------------------------------------
--- Instances
-----------------------------------------------------------

derive instance Functor HTML
