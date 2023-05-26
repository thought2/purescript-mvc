module MVC.Record.Init
  ( class InitRecord
  , initRecord
  ) where

import MVC.Record (RecordState(..))

class InitRecord inits rsta where
  initRecord :: Record inits -> RecordState rsta

instance InitRecord inits inits where
  initRecord = RecordState