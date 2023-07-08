{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.Targets.Aeson.Instances where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Ondim
import qualified Data.Aeson.Key as K
import Data.Typeable ((:~:)(..), eqT)

instance Conversible Array [Value] where
  convertTo = toList
  updateFrom _ = fromList

instance OndimNode Value where
  type
    ExpTypes Value =
      '[ ToSpecSel ('ConsSel "Object") (Trav KM.KeyMap (Nesting Value)),
         ToSpecSel ('ConsSel "String") Text,
         ToSpecSel ('ConsSel "Array") (Converting Array Value)
       ]
  identify (Object o)
    | Just (String name) <- KM.lookup "$" o = Just name
  identify _ = Nothing
  children (Object o)
    | Just (Array a) <- KM.lookup "$args" o = toList a
  children _ = []
  attributes (Object o) = liftNodes $ KM.foldrWithKey go [] o
    where
      go k (String t) a = (K.toText k, t) : a
      go _ _ a = a
  attributes _ = pure []
  castFrom (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just $ one . String
    | otherwise = Nothing
  castTo (_ :: Proxy t)
    | Just Refl <- eqT @t @Text = Just \case
        String t -> [t]
        _notString -> mempty
    | Just Refl <- eqT @t @Rendered = Just $ one . encode
    | otherwise = Nothing