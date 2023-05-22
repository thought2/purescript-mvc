module MVC.Types where

type UI srf msg sta =
  { init :: sta
  , update :: msg -> sta -> sta
  , view :: sta -> srf msg
  }