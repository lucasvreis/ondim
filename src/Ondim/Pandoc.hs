{-# OPTIONS_GHC -Wno-orphans #-}
-- | Experimental Pandoc (or rather, untyped Pandoc JSON) support

module Ondim.Pandoc where
import Ondim
import Data.Aeson.Types
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Relude.Extra.Lens
import Relude.Extra.Map (toPairs)

-- | The JSON representation is used instead of the AST because it is more
-- flexible. If <https://github.com/jgm/pandoc-types/pull/99> is merged it *may*
-- be possible/desirable to directly use the AST.
newtype PandocJSON = PandocJSON { unPandocJSON :: Value }

type OndimPandocT m a = OndimT PandocJSON m a

instance OndimNode PandocJSON where
  identify (unPandocJSON -> Object o)
    | Just (String name) <- KM.lookup "key" o = Just name
  identify _ = Nothing
  children = lens getChild setChild
    where
      getChild (unPandocJSON -> Object o)
        | Just (Array a) <- KM.lookup "c" o = map PandocJSON (toList a)
      getChild (unPandocJSON -> Array a) = map PandocJSON (toList a)
      getChild _ = []
      setChild (unPandocJSON -> Object o) (map unPandocJSON -> a)
        | Just (Array _) <- KM.lookup "c" o =
            PandocJSON $
              Object $ KM.insert "c" (Array (fromList a)) o
      setChild (unPandocJSON -> Array _) (map unPandocJSON -> a) =
        PandocJSON $ Array (fromList a)
      setChild n _ = n
  attributes = lens getAttr setAttr
    where
      getAttr (unPandocJSON -> Object o)
        | KM.member "key" o = mapMaybe toAttr (toPairs o)
        where
          toAttr (K.toText -> k, String v)
            | k /= "key", k /= "c", k /= "t" = Just (k, v)
          toAttr _ = Nothing
      getAttr _ = []
      setAttr (unPandocJSON -> Object o) a =
        PandocJSON $
          Object $ KM.fromList (map (bimap K.fromText String) a) <> o
      setAttr n _ = n
  asText = asText' . unPandocJSON
    where
      asText' (String t) = t
      asText' (Array v) = foldMap asText' v
      asText' (Object v) = foldMap asText' v
      asText' (Number v) = show v
      asText' (Bool v) = show v
      asText' _ = ""
  injText t = PandocJSON (String t)
