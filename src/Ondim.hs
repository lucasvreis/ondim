{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Ondim
  ( OndimNode (..)
  , OndimS (..)
  , Expansion
  , Expansions
  , Expansions'
  , AttrExpansions'
  , AttrExpansion
  , AttrExpansions
  , emptyOndimS
  , OndimException (..)
  , OndimT
  , runOndimT
  , withOndimS
  , getExpansion
  , liftNode
  , withExpansions
  , putExpansion
  , withAttrExpansions
  , bindingExpansions
  , bindingAttrExpansions
  , runChildrenWith
  , fromTemplate
  , callExpansion
  )
  where
import Relude.Extra.Map
import Data.Map.Syntax
import Control.Monad.Except
import Relude.Extra.Lens
import Data.Attoparsec.Text (char, Parser, string, takeTill)
import Replace.Attoparsec.Text (streamEditT)

class OndimNode t where
  identify :: t -> Maybe Text
  children :: Lens' t [t]
  attributes :: Lens' t [(Text, Text)]
  asText :: t -> Text
  injText :: Text -> t

type Expansion m t = OndimT t m t -> OndimT t m [t]
type Expansions m t = Map Text (Expansion m t)

type AttrExpansion m t = Text -> OndimT t m [(Text, Text)]
type AttrExpansions m t = Map Text (AttrExpansion m t)

data OndimS m t = OndimS
  { expansions :: Expansions m t
  , attrExpansions :: AttrExpansions m t
  , expansionDepth :: Int
  , expansionTrace :: [Text]
  , afterExpansion :: [t] -> [t]
  }

emptyOndimS :: OndimS m t
emptyOndimS = OndimS mempty mempty 0 [] id

newtype OndimException
  = MaxExpansionDepthExceeded [Text]
  deriving (Show)

newtype OndimT t m a = OndimT { unOndimT :: StateT (OndimS m t) (ExceptT OndimException m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadError OndimException)

instance MonadTrans (OndimT t) where
  lift = OndimT . lift . lift

instance MonadState s m => MonadState s (OndimT t m) where
    get = lift get
    put = lift . put
    state = lift . state

runOndimT :: Monad m => OndimT t m a -> OndimS m t -> m (Either OndimException a)
runOndimT o = runExceptT . evalStateT (unOndimT o)

{- | This function works like @withReaderT@, in the sense that it creates a new
   scope for the state in which state changes do not leak outside.
-}
withStateT' :: Monad m => (t -> s) -> StateT s m a -> StateT t m a
withStateT' f st =
  StateT \s -> (, s) <$> evalStateT st (f s)

withOndimS :: Monad m =>
  (OndimS m t -> OndimS m t) ->
  OndimT t m a ->
  OndimT t m a
withOndimS f = OndimT . withStateT' f . unOndimT

getExpansion :: Monad m => Text -> OndimT t m (Maybe (Expansion m t))
getExpansion k = OndimT $
  gets (\s -> (fmap (afterExpansion s) .) <$> lookup k (expansions s))

-- Substitution of ${name} in attribute text

interpParser :: Parser Text
interpParser = do
  _ <- string "${"
  s <- takeTill (== '}')
  _ <- char '}'
  pure s

interpEditor :: (Monad m, OndimNode t) => Text -> OndimT t m Text
interpEditor t = do
  expansion <- getExpansion t
  fromMaybe (pure t) $ do
    expansion' <- expansion
    pure $ foldMap asText <$> expansion' (pure (injText ""))

attrEdit :: (Monad m, OndimNode t) => Text -> OndimT t m Text
attrEdit = streamEditT interpParser interpEditor

expandAttr :: (Monad m, OndimNode t) => (Text, Text) -> OndimT t m [(Text, Text)]
expandAttr attr = do
  st <- OndimT $ get
  mapM (\(x,y) -> (x,) <$> attrEdit y) =<<
    fromMaybe (pure [attr]) do
      expansion <- lookup (fst attr) (attrExpansions st)
      pure $ expansion (snd attr)

-- | This function recursively lifts the nodes into an unvaluated state, that
-- will be evaluated with the defined expansions.
liftNode :: forall m t. (Monad m, OndimNode t) => t -> OndimT t m [t]
liftNode node = do
  let attr = node ^. attributes
      child = node ^. children
      liftedNode = do
        attr'  <- foldMapM expandAttr attr
        child' <- withOndimS id $ -- Shield the inner scope
                  foldMapM liftNode child
        pure $ node &
          set attributes attr' &
          set children child'
  st <- OndimT $ get
  fromMaybe (one <$> liftedNode) do
    name      <- identify node
    expansion <- lookup name (expansions st)
    pure do
      when (expansionDepth st >= 200) $ -- To avoid recursive expansions
        throwError (MaxExpansionDepthExceeded $ expansionTrace st)
      withOndimS (\s -> s { expansionDepth = expansionDepth st + 1
                         , expansionTrace = show name : expansionTrace st}) $
        afterExpansion st <$> expansion liftedNode

-- | "Bind" new expansions.
withExpansions :: (Monad m, OndimNode t) => Expansions m t -> OndimT t m a -> OndimT t m a
withExpansions exps = withOndimS (\s -> s {expansions = exps <> expansions s})

-- | Put a new expansion into the local state, modifying it.
putExpansion :: (Monad m, OndimNode t) => Text -> Expansion m t -> OndimT t m ()
putExpansion key exps = OndimT $ modify (\s -> s {expansions = insert key exps (expansions s)})

-- | "Bind" new attr expansions.
withAttrExpansions :: (Monad m, OndimNode t) => AttrExpansions m t -> OndimT t m a -> OndimT t m a
withAttrExpansions exps = withOndimS (\s -> s {attrExpansions = exps <> attrExpansions s})

type Expansions' m t = MapSyntax Text (Expansion m t)

type AttrExpansions' m t = MapSyntax Text (AttrExpansion m t)

-- | Convenience function to bind using MapSyntax.
bindingExpansions :: (Monad m, OndimNode t) =>
  OndimT t m a -> Expansions' m t -> OndimT t m a
bindingExpansions o exps = withExpansions (fromRight mempty (runMap exps)) o

-- | Convenience function to bind using MapSyntax.
bindingAttrExpansions :: (Monad m, OndimNode t) =>
  OndimT t m a -> AttrExpansions' m t -> OndimT t m a
bindingAttrExpansions o exps = withAttrExpansions (fromRight mempty (runMap exps)) o

runChildrenWith ::
  (Monad m, OndimNode t) =>
  Expansions' m t -> Expansion m t
runChildrenWith exps node = (view children <$> node) `bindingExpansions` exps

fromTemplate ::
  (Monad m, OndimNode t) =>
  [t] -> Expansion m t
fromTemplate tpl inner =
  foldMapM liftNode tpl `bindingExpansions` do
    "apply-content" ## const (view children <$> inner)

callExpansion ::
  (Monad m, OndimNode t)
  => Text -> OndimT t m [t]
callExpansion name = do
  exps <- getExpansion name
  maybe (pure []) ($ pure (injText "")) exps
