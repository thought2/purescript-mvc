module Test.MVC.Variant.UpdateSpec where

import Prelude

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Data.Variant as V
import MVC.Variant.Types (VariantMsg(..), VariantState(..))
import MVC.Variant.Update (matchCase, matchCaseRL, updateRL, updateVariant)
import Prim.RowList as RL
import Test.MVC.TestTypes (M1(..), M2, M3, S1(..), S2(..), S3(..), T1, T2, T3)
import Type.Function (type (#), type ($))
import Type.Proxy (Proxy(..))

testUpdate1 :: VariantState (case1 :: S1, case2 :: S2, case3 :: S3)
testUpdate1 = updateVariant
  { case1: S1
  , case2: S2
  , case3: S3
  }
  { case1: \(_ :: M1) (_ :: S1) -> S1
  , case2: \(_ :: M2) (_ :: S2) -> S2
  , case3: \(_ :: M3) (_ :: S3) -> S3
  }
  (ChildCaseMsg (V.inj (Proxy :: Proxy "case1") M1))
  (VariantState (V.inj (Proxy :: Proxy "case1") S1))

testUpdate2 :: VariantState (case1 :: S1, case2 :: S2, case3 :: S3)
testUpdate2 = updateVariant
  { case1: S1
  , case2: S2
  , case3: S3
  }
  { case1: \(_ :: M1) (_ :: S1) -> S1
  , case2: \(_ :: M2) (_ :: S2) -> S2
  , case3: \(_ :: M3) (_ :: S3) -> S3
  }
  (ChangeCase (V.inj (Proxy :: Proxy "case1") unit))
  (VariantState (V.inj (Proxy :: Proxy "case1") S1))

testUpdateRL :: Variant (case1 :: S1, case2 :: S2, case3 :: S3)
testUpdateRL = updateRL
  ( Proxy
      :: _ $ RL.Nil
           # RL.Cons "case1" Unit
           # RL.Cons "case2" Unit
           # RL.Cons "case3" Unit
  )
  { case1: \(_ :: M1) (_ :: S1) -> S1
  , case2: \(_ :: M2) (_ :: S2) -> S2
  , case3: \(_ :: M3) (_ :: S3) -> S3
  }
  (V.inj (Proxy :: Proxy "case1") M1)
  (V.inj (Proxy :: Proxy "case1") S1)

testMatchCase :: Variant (case1 :: S1, case2 :: S2, case3 :: S3)
testMatchCase = matchCase
  { case1: S1
  , case2: S2
  , case3: S3
  }
  (V.inj (Proxy :: Proxy "case2") unit)

testMatchCaseRL :: Variant (case1 :: S1, case2 :: S2, case3 :: S3)
testMatchCaseRL = matchCaseRL
  ( Proxy
      :: _ $ RL.Nil
           # RL.Cons "case3" Unit
           # RL.Cons "case2" Unit
           # RL.Cons "case1" Unit
  )
  { case1: S1
  , case2: S2
  , case3: S3
  }
  (V.inj (Proxy :: Proxy "case2") unit)

