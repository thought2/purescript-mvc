module Test.MVC.TestTypes where

import Prelude

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
