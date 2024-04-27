{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Ondim.Internal.Class
  ( OndimNode (..),
    Expansible (..),
    ondimCast,
    Attribute,
  ) where

import Data.Typeable (eqT, (:~:) (..))
import {-# SOURCE #-} Ondim.Internal.Basic (Ondim)

-- * Attributes

-- | Alias for attributes
type Attribute = (Text, Text)

-- ** Class

-- * 'OndimNode' class

class Expansible (t :: Type) where
  -- | Expand only the substructures of a node.
  expandSubs :: t -> Ondim s t
  expandSubs = return

class (Expansible t, OndimCast t) => OndimNode t where
  -- | Returns the name of the node as defined by the 'OndimNode' instance.
  identify :: t -> Maybe Text
  identify _ = Nothing

  -- | Returns a list of attributes of the node as defined by the 'OndimNode' instance.
  attributes :: t -> Ondim s [Attribute]
  attributes _ = pure []

  -- | Returns the children of the node as defined by the 'OndimNode' instance.
  children :: t -> [t]
  children _ = []

  castFrom :: (Typeable a) => Maybe (a -> [t])
  castFrom = Nothing

  -- | Converts the node to a 'LByteString' as defined by the 'OndimNode' instance.
  renderNode :: Maybe (t -> LByteString)
  renderNode = Nothing

  nodeAsText :: Maybe (t -> Text)
  nodeAsText = Nothing

instance (OndimNode a, Expansible (t a), Foldable t, Typeable t) => OndimNode (t a) where
  renderNode = foldMap' <$> renderNode
  nodeAsText = foldMap' <$> nodeAsText

-- Some data instances (won't lift)

instance (Expansible Text)
instance OndimNode Text where
  nodeAsText = Just id

instance (Expansible LByteString)
instance OndimNode LByteString

class (Typeable a) => OndimCast a where
  ondimCast :: (OndimNode b) => Maybe (a -> [b])

instance {-# OVERLAPPABLE #-} (Typeable a) => OndimCast a where
  ondimCast :: forall b. (OndimNode b) => Maybe (a -> [b])
  ondimCast = castFrom

instance (OndimCast a, Typeable t, Foldable t) => OndimCast (t a) where
  ondimCast :: forall b. (OndimNode b) => Maybe (t a -> [b])
  ondimCast
    | Just Refl <- eqT @b @a = Just toList
    | otherwise = castFrom <|> foldMap' <$> ondimCast

-- ondimCast :: forall a b. (OndimNode a, OndimNode b) => Maybe (a -> [b])
-- ondimCast = castTo (Proxy @b) <|> castFrom (Proxy @a)
