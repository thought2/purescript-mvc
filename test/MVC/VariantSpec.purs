module Test.MVC.VariantSpec where

import Prelude


-- testView :: HTML (Msg (case2 :: Unit, case3 :: Unit, case1 :: Unit) (case2 :: M2, case3 :: M3, case1 :: M1))
-- testView = view
--   { viewUser:
--       \sel mk xs ->
--         VE.div_
--           [ VE.select [ E.onChange (CaseKey >>> mk) ] $
--               xs <#> \x -> VE.option [ VA.value (NT.un CaseKey x) ] [ text (NT.un CaseKey x) ]
--           , sel
--           ]
--   }
--   { case1: \(_ :: S1) -> HTML :: _ M1
--   , case2: \(_ :: S2) -> HTML :: _ M2
--   , case3: \(_ :: S3) -> HTML :: _ M3
--   }
--   (State $ V.inj (Proxy :: Proxy "case1") S1)



-- testCaseKeyToVariantRL :: Maybe (Variant (case1 :: Unit, case2 :: Unit, case3 :: Unit))
-- testCaseKeyToVariantRL = caseKeyToVariantRL
--   ( Proxy
--       :: _ $ RL.Nil
--            # RL.Cons "case3" Unit
--            # RL.Cons "case2" Unit
--            # RL.Cons "case1" Unit
--   )
--   (CaseKey "case2")


-- testViewRL :: HTML (Variant (case2 :: M2, case3 :: M3, case1 :: M1))
-- testViewRL = viewRL
--   ( Proxy
--       :: _ $ RL.Nil
--            # RL.Cons "case1" Unit
--            # RL.Cons "case2" Unit
--            # RL.Cons "case3" Unit
--   )
--   { case1: \(_ :: S1) -> HTML :: _ M1
--   , case2: \(_ :: S2) -> HTML :: _ M2
--   , case3: \(_ :: S3) -> HTML :: _ M3
--   }
--   (V.inj (Proxy :: Proxy "case1") S1)


-- testGetKeys :: Array String
-- testGetKeys = getKeys
--   ( Proxy
--       :: _
--            ( RL.Nil
--                # RL.Cons "c" Unit
--                # RL.Cons "b" Unit
--                # RL.Cons "a" Unit
--            )
--   )
