{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}

module Ondim
  ( OndimTag (..)
  , HasSub (..)
  , OneSub (..)
  , children
  , OndimNode (..)
  , Expansion
  , Expansions
  , OndimGS (..)
  , initialOGS
  , OndimS (..)
  , initialOS
  , OndimException (..)
  , throwNotBound
  , Ondim
  , liftO
  , runOndimT
  , withOndimGS
  , inhibitingExpansions
  , withOndimS
  , getExpansion
  , liftNode
  , withExpansions
  , withText
  , putExpansion
  , putTextExp
  , Expansions'
  , binding
  , bindingText
  , fromTemplate
  , callExpansion
  , callText
  )
  where
import Prelude hiding (All)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans.MultiState.Strict (runMultiStateTA, mGet, MultiStateT (..))
import Ondim.MultiState (All, mGets, mModify, withMultiStateT)
import Data.HList.ContainsType (ContainsType)
import Data.HList.HList (HList (..))
import Relude.Extra.Map (insert, lookup)
import Data.Map.Syntax ((##), runMap, MapSyntax)
import Ondim.HasSub

class Monad (OndimMonad t) => OndimTag t where
  type OndimTypes t :: [Type]
  type OndimMonad t :: Type -> Type

children :: forall t tag. (OndimTag tag, HasSub tag t t) =>
  Expansion tag t
children = fmap (getSubs @tag)

class
  ( All (OndimNode tag) (ExpTypes t)
  , All (HasSub tag t) (ExpTypes t)
  , All (ContainsOndimS tag) (ExpTypes t)
  , All LiftAllSub (AllExpTypes (ExpTypes t))
  , ContainsOndimS tag t
  , LiftAllSub (ExpTypes t)
  , ContainsType t (OndimTypes tag)
  , OndimTag tag
  ) => OndimNode tag t
  where
  type ExpTypes t :: [Type]
  identify :: t -> Maybe Text
  identify _ = Nothing
  fromText :: Maybe (Text -> t)
  fromText = Nothing
  validIdentifiers :: Maybe [Text]
  validIdentifiers = Nothing

instance
  ( HasSub tag [t] t
  , ContainsOndimS tag [t]
  , ContainsType [t] (OndimTypes tag)
  , OndimNode tag t
  ) => OndimNode tag [t] where
  type ExpTypes [t] = '[t]
  identify (x : _) = identify @tag x
  identify _ = Nothing
  fromText = (one .) <$> fromText @tag
  validIdentifiers = validIdentifiers @tag @t

type Expansion tag t = Ondim tag t -> Ondim tag [t]
type Expansions tag t = Map Text (Expansion tag t)

{- | Ondim's global state
-}
data OndimGS tag = OndimGS
 { expansionDepth :: Int
 , expansionTrace :: [Text]
 , inhibitExpansion :: Bool
 , textExpansions :: Map Text (Ondim tag Text)
 }

{- | Initial global state
-}
initialOGS :: OndimGS tag
initialOGS = OndimGS 0 [] False mempty

{- | Ondim's state (one for each type)
-}
newtype OndimS tag t = OndimS
  { expansions :: Expansions tag t
  }

{- | Initial state
-}
initialOS :: OndimS tag t
initialOS = OndimS mempty

type family MultiOndimS' tag (l :: [Type]) where
  MultiOndimS' tag '[] = '[]
  MultiOndimS' tag (l : ls) = OndimS tag l : MultiOndimS' tag ls

type MultiOndimS tag = MultiOndimS' tag (OndimTypes tag)

data OndimException
  = MaxExpansionDepthExceeded [Text]
  | ExpansionNotBound Text [Text]
  deriving (Show)

throwNotBound :: forall tag b.
  OndimTag tag =>
  Text -> Ondim tag b
throwNotBound name =
  throwError . ExpansionNotBound name
    =<< Ondim (mGets @(OndimGS tag) expansionTrace)

newtype Ondim tag a = Ondim
  { unOndimT ::
      MultiStateT
        (OndimGS tag : MultiOndimS tag)
        (ExceptT OndimException (OndimMonad tag)) a
  }

deriving instance (Functor (OndimMonad tag)) => (Functor (Ondim tag))
deriving newtype instance (Monad (OndimMonad tag)) => (Applicative (Ondim tag))
deriving newtype instance (Monad (OndimMonad tag)) => (Monad (Ondim tag))
deriving newtype instance (MonadIO (OndimMonad tag)) => (MonadIO (Ondim tag))
deriving newtype instance (MonadState s (OndimMonad tag)) => (MonadState s (Ondim tag))

liftO :: (OndimTag tag) => (OndimMonad tag) a -> Ondim tag a
liftO = Ondim . lift . lift

instance Monad (OndimMonad tag) => MonadError OndimException (Ondim tag) where
  throwError = Ondim . MultiStateT . throwError
  catchError m h = to' $ catchError (from m) (from . h)
    where to' = Ondim . MultiStateT
          from = runMultiStateTRaw . unOndimT

class HasInitialMultiState (ls :: [Type]) where
  {- | Initial multi state
  -}
  initialOMS :: HList (MultiOndimS' tag ls)

instance HasInitialMultiState '[] where
  initialOMS = HNil

instance HasInitialMultiState ls => HasInitialMultiState (l : ls) where
  initialOMS :: forall tag. HList (MultiOndimS' tag (l : ls))
  initialOMS = initialOS :+: initialOMS @ls @tag

{- | Runs the Ondim action
-}
runOndimT ::
  forall tag a.
  ( OndimTag tag
  , HasInitialMultiState (OndimTypes tag)
  ) =>
  Ondim tag a -> (OndimMonad tag) (Either OndimException a)
runOndimT o = runExceptT $
  runMultiStateTA (initialOGS :+: initialOMS @(OndimTypes tag) @tag) (unOndimT o)

class ContainsType (OndimS tag t) (MultiOndimS tag) =>
  ContainsOndimS tag t

instance ContainsType (OndimS tag t) (MultiOndimS tag) =>
  ContainsOndimS tag t

withOndimGS :: (OndimTag tag) =>
  (OndimGS tag -> OndimGS tag) ->
  Ondim tag a ->
  Ondim tag a
withOndimGS f = Ondim . withMultiStateT f . unOndimT

inhibitingExpansions :: OndimTag tag => Ondim tag a -> Ondim tag a
inhibitingExpansions = withOndimGS (\s -> s { inhibitExpansion = True })

withOndimS :: forall t tag a. (OndimTag tag, ContainsOndimS tag t) =>
  (OndimS tag t -> OndimS tag t) ->
  Ondim tag a ->
  Ondim tag a
withOndimS f = Ondim . withMultiStateT f . unOndimT

getExpansion :: forall t tag.
  (OndimTag tag, ContainsOndimS tag t) =>
  Text -> Ondim tag (Maybe (Expansion tag t))
getExpansion k = Ondim $
  mGets (\s -> lookup k (expansions s))

liftSub ::
  forall tag t s.
  ( HasSub tag t s
  , OndimNode tag s
  , ContainsOndimS tag s
  , LiftAllSub (ExpTypes s)
  ) =>
  t -> Ondim tag t
liftSub node =
  let child = withOndimGS id $
        foldMapM (liftNode @tag) (getSubs @tag @t @s node)
  in setSubs @tag node <$> child
{-# INLINABLE liftSub #-}

type family AllExpTypes (ls :: [Type]) :: [[Type]] where
  AllExpTypes '[] = '[]
  AllExpTypes (t : ts) = ExpTypes t : AllExpTypes ts

class LiftAllSub (ls :: [Type]) where
  liftAllSub ::
    forall tag t.
    ( OndimTag tag
    , All (HasSub tag t) ls
    , All (ContainsOndimS tag) ls
    , All (OndimNode tag) ls
    , All LiftAllSub (AllExpTypes ls)
    ) =>
    t -> Ondim tag t

instance LiftAllSub '[] where
  liftAllSub = pure

instance
  ( LiftAllSub xs
  ) => LiftAllSub (x : xs)
  where
  liftAllSub ::
    forall tag t.
    ( All (HasSub tag t) (x : xs)
    , All (ContainsOndimS tag) (x : xs)
    , All (OndimNode tag) (x : xs)
    , All LiftAllSub (ExpTypes x : AllExpTypes xs)
    ) =>
    t -> Ondim tag t
  liftAllSub = liftSub @tag @t @x >=> liftAllSub @xs

{- | This function recursively lifts the nodes into an unvaluated state, that will
   be evaluated with the defined expansions.
-}
liftNode ::
  forall tag t.
  ( ContainsOndimS tag t
  , OndimNode tag t
  , LiftAllSub (ExpTypes t)
  ) =>
  t -> Ondim tag [t]
liftNode node = do
  gst <- Ondim $ mGet @(OndimGS tag)
  st  <- Ondim $ mGet @(OndimS tag t)
  if | inhibitExpansion gst -> pure (one node)
     | Just name <- identify @tag node ->
       if | expansionDepth gst >= 200 -> -- To avoid recursive expansions
              throwError (MaxExpansionDepthExceeded $ expansionTrace gst)
          | Just expansion <- lookup name (expansions st) ->
              expansion $
                expCtx name $
                  liftedNode
          | Just fT <- fromText @tag,
            Just text <- lookup name (textExpansions gst) ->
              expCtx name $
                one . fT <$> text
          | Just valid <- validIdentifiers @tag @t,
            name `notElem` valid -> throwNotBound name
          | otherwise -> one <$> liftedNode
      | otherwise -> one <$> liftedNode
  where
    liftedNode = liftAllSub @(ExpTypes t) node
    expCtx name =
      withOndimGS
        (\s -> s { expansionDepth = expansionDepth s + 1
                 , expansionTrace = name : expansionTrace s })

-- | "Bind" new expansions locally.
withExpansions :: OndimNode tag t => Expansions tag t -> Ondim tag a -> Ondim tag a
withExpansions exps = withOndimS (\s -> s {expansions = exps <> expansions s})

-- | "Bind" text expansions locally.
withText :: OndimTag tag => Map Text (Ondim tag Text) -> Ondim tag a -> Ondim tag a
withText exps = withOndimGS (\s -> s {textExpansions = exps <> textExpansions s})

-- | Put a new expansion into the local state, modifying the scope.
putExpansion :: OndimNode tag t => Text -> Expansion tag t -> Ondim tag ()
putExpansion key exps =
  Ondim $ mModify (\s -> s {expansions = insert key exps (expansions s)})

-- | Put a new expansion into the local state, modifying the scope.
putTextExp :: OndimTag tag => Text -> Ondim tag Text -> Ondim tag ()
putTextExp key exps =
  Ondim $ mModify (\s -> s {textExpansions = insert key exps (textExpansions s)})

type Expansions' m t = MapSyntax Text (Expansion m t)

-- | Convenience function to bind using MapSyntax.
binding :: OndimNode tag t =>
  Ondim tag a -> Expansions' tag t -> Ondim tag a
binding o exps = withExpansions (fromRight mempty (runMap exps)) o

-- | Convenience function to bind using MapSyntax.
bindingText :: OndimTag tag =>
  Ondim tag a -> MapSyntax Text (Ondim tag Text) -> Ondim tag a
bindingText o exps = withText (fromRight mempty (runMap exps)) o

fromTemplate :: forall tag t.
  (OndimNode tag t, LiftAllSub (ExpTypes t), HasSub tag t t) =>
  [t] -> Expansion tag t
fromTemplate tpl inner =
  foldMapM liftNode tpl `binding` do
    "apply-content" ## const (children inner)

{- | Either applies expansion 'name', or throws an error if it does not exist.
-}
callExpansion :: OndimNode tag t => Text -> Expansion tag t
callExpansion name arg = do
  exps <- getExpansion name
  maybe (throwNotBound name) ($ arg) exps

callText ::
  OndimTag tag =>
  Text -> Ondim tag Text
callText k =
  fromMaybe (throwNotBound k) =<<
    Ondim (mGets \s -> lookup k (textExpansions s))