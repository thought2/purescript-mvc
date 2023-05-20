module MVC.Record.Types where

import Data.Variant (Variant)

newtype RecordState r = RecordState (Record r)

data RecordMsg r = Set (Variant r)

