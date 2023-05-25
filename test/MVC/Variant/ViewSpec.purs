module Test.MVC.Variant.ViewSpec where

import Prelude

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Data.Variant as V
import MVC.Variant.Types (CaseKey(..), VariantMsg, VariantState(..))
import MVC.Variant.View (caseKeyToVariantRL, getKeys, viewRL, viewVariant)
import Prim.RowList as RL
import Test.MVC.TestTypes (HTML(..), M1, M2, M3, S1(..), S2, S3)
import Type.Function (type (#), type ($))
import Type.Proxy (Proxy(..))

testView
  :: HTML
       ( VariantMsg
           ( case1 :: Unit
           , case2 :: Unit
           , case3 :: Unit
           )
           ( case1 :: M1
           , case2 :: M2
           , case3 :: M3
           )
       )
testView = viewVariant
  { viewUser: \_ _ _ -> HTML
  }
  { case1: \(_ :: S1) -> HTML :: _ M1
  , case2: \(_ :: S2) -> HTML :: _ M2
  , case3: \(_ :: S3) -> HTML :: _ M3
  }
  (VariantState $ V.inj (Proxy :: Proxy "case1") S1)

testCaseKeyToVariantRL
  :: Maybe
       ( Variant
           ( case1 :: Unit
           , case2 :: Unit
           , case3 :: Unit
           )
       )
testCaseKeyToVariantRL = caseKeyToVariantRL
  ( Proxy
      :: _ $ RL.Nil
           # RL.Cons "case3" Unit
           # RL.Cons "case2" Unit
           # RL.Cons "case1" Unit
  )
  (CaseKey "case2")

testViewRL
  :: HTML
       ( Variant
           ( case1 :: M1
           , case2 :: M2
           , case3 :: M3
           )
       )
testViewRL = viewRL
  ( Proxy
      :: _ $ RL.Nil
           # RL.Cons "case1" Unit
           # RL.Cons "case2" Unit
           # RL.Cons "case3" Unit
  )
  { case1: \(_ :: S1) -> HTML :: _ M1
  , case2: \(_ :: S2) -> HTML :: _ M2
  , case3: \(_ :: S3) -> HTML :: _ M3
  }
  (V.inj (Proxy :: Proxy "case1") S1)

testGetKeys :: Array String
testGetKeys = getKeys
  ( Proxy
      :: _
           ( RL.Nil
               # RL.Cons "c" Unit
               # RL.Cons "b" Unit
               # RL.Cons "a" Unit
           )
  )
