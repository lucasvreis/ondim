module Ondim.Targets.Aeson.Expansions where

import Ondim
import Ondim.Extra.Standard (standardMap)
import Ondim.Targets.Aeson.Instances ()

defaultState :: Monad m => OndimState m
defaultState =
  OndimState { expansions = exps
             }
  where
    exps = mapToNamespace standardMap
