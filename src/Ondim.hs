{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}

module Ondim
  ( OndimTag (..)
  , HasSub (..)
  , OneSub (..)
  , PairSub (..)
  , MapSub
  , NestedSub (..)
  , children
  , OndimNode (..)
  , Expansion
  , Expansions
  , Filter
  , Filters
  , OndimGS (..)
  , initialOGS
  , OndimS (..)
  , initialOS
  , OndimMS
  , ondimState
  , ondimGState
  , initialMS
  , OndimException (..)
  , throwNotBound
  , Ondim
  , ContainsOndimS
  , liftO
  , runOndimTWith
  , runOndimT
  , withOndimGS
  , inhibitingExpansions
  , withOndimS
  , getExpansion
  , getTextExpansion
  , liftNode
  , liftNodes
  , liftSubstructures
  , withExpansions
  , withFilters
  , withText
  , withoutExpansion
  , withoutFilter
  , withoutText
  , putExpansion
  , putTextExp
  , Expansions'
  , Filters'
  , binding
  , bindingFilters
  , bindingText
  , fromTemplate
  , callExpansion
  , callText
  )
  where
import Prelude hiding (All)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans.MultiState.Strict (runMultiStateTA, mGet, MultiStateT (..), runMultiStateT)
import Ondim.MultiState (All, mGets, mModify, withMultiStateT)
import Data.HList.ContainsType (ContainsType, setHListElem, getHListElem)
import Data.HList.HList (HList (..))
import Relude.Extra.Map (insert, lookup, delete, keys)
import Data.Map.Syntax ((##), runMap, MapSyntax)
import Ondim.HasSub
import Data.Typeable (TypeRep, typeRep)
import Relude.Extra.Lens

class
  ( Monad (OndimMonad t)
  , HasInitialMultiState (OndimTypes t)
  ) => OndimTag t
  where
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
  , Typeable t
  ) => OndimNode tag t
  where
  type ExpTypes t :: [Type]
  identify :: t -> Maybe Text
  identify _ = Nothing
  fromText :: Maybe (Text -> [t])
  fromText = Nothing
  asText :: Maybe (t -> Text)
  asText = Nothing
  validIdentifiers :: Maybe [Text]
  validIdentifiers = Nothing

type Expansion tag t = Ondim tag t -> Ondim tag [t]
type Expansions tag t = Map Text (Expansion tag t)

type Filter tag t = Ondim tag [t] -> Ondim tag [t]
type Filters tag t = Map Text (Filter tag t)

{- | Ondim's global state
-}
data OndimGS tag = OndimGS
 { expansionDepth :: Int
 , expansionTrace :: [Text]
 , inhibitExpansion :: Bool
 , textExpansions :: Expansions tag Text
 }
 deriving (Generic)

{- | Initial global state
-}
initialOGS :: OndimGS tag
initialOGS = OndimGS 0 [] False mempty

{- | Ondim's state (one for each type)
-}
data OndimS tag t = OndimS
  { expansions :: Expansions tag t
  -- ^ Named expansions
  , filters :: Filters tag t
  -- ^ Similar to expansions, but are always applied after the expansion (the
  -- purpose of the name is just to facilitate binding/unbinding).
  }
  deriving (Generic)

instance Semigroup (OndimS tag t) where
  OndimS x1 y1 <> OndimS x2 y2 = OndimS (x1 <> x2) (y1 <> y2)

instance Monoid (OndimS tag t) where
  mempty = OndimS mempty mempty

{- | Initial state
-}
initialOS :: OndimS tag t
initialOS = OndimS mempty mempty

type family MultiOndimS' tag (l :: [Type]) where
  MultiOndimS' tag '[] = '[]
  MultiOndimS' tag (l : ls) = OndimS tag l : MultiOndimS' tag ls

type MultiOndimS tag = MultiOndimS' tag (OndimTypes tag)

newtype OndimMS tag = OndimMS (HList (OndimGS tag : MultiOndimS tag))
 deriving (Generic)

ondimState :: forall t tag. OndimNode tag t => Lens' (OndimMS tag) (OndimS tag t)
ondimState = lens getMS setMS
  where
    setMS (OndimMS ms) s = OndimMS $ setHListElem s ms
    getMS (OndimMS ms) = getHListElem ms

ondimGState :: forall tag. Lens' (OndimMS tag) (OndimGS tag)
ondimGState = lens getGS setGS
  where
    setGS (OndimMS ms) s = OndimMS $ setHListElem s ms
    getGS (OndimMS ms) = getHListElem ms

class ConcatHLists (ls :: [Type]) where
  concatHLists :: HList ls -> HList ls -> HList ls

instance ConcatHLists '[] where
  concatHLists _ _ = HNil

instance (ConcatHLists ls, Monoid l) => ConcatHLists (l : ls) where
  concatHLists (a :+: as) (b :+: bs) = (a <> b) :+: concatHLists as bs

instance ConcatHLists (MultiOndimS tag) => Semigroup (OndimMS tag) where
  OndimMS (s :+: x) <> OndimMS (_ :+: y) = OndimMS (s :+: concatHLists x y)

data OndimException
  = MaxExpansionDepthExceeded [Text]
  | ExpansionNotBound Text TypeRep [Text]
  deriving (Show)

throwNotBound :: forall t tag s.
  (OndimTag tag, Typeable t) =>
  Text -> Ondim tag s
throwNotBound name =
  throwError . ExpansionNotBound name (typeRep (Proxy @t))
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
  initialOMS :: HList (MultiOndimS' tag ls)

instance HasInitialMultiState '[] where
  initialOMS = HNil

instance HasInitialMultiState ls => HasInitialMultiState (l : ls) where
  initialOMS :: forall tag. HList (MultiOndimS' tag (l : ls))
  initialOMS = initialOS :+: initialOMS @ls @tag

initialMS :: forall tag. HasInitialMultiState (OndimTypes tag) => OndimMS tag
initialMS = OndimMS (initialOGS :+: initialOMS @(OndimTypes tag) @tag)

{- | Runs the Ondim action with a given initial state.
-}
runOndimTWith ::
  forall tag a.
  ( OndimTag tag
  ) =>
  OndimMS tag -> Ondim tag a -> (OndimMonad tag) (Either OndimException a)
runOndimTWith (OndimMS s) o = runExceptT $
  runMultiStateTA s (unOndimT o)

{- | Runs the Ondim action with empty initial state.
-}
runOndimT ::
  forall tag a.
  ( OndimTag tag
  , HasInitialMultiState (OndimTypes tag)
  ) =>
  Ondim tag a -> (OndimMonad tag) (Either OndimException a)
runOndimT = runOndimTWith initialMS

class ContainsType (OndimS tag t) (MultiOndimS tag) =>
  ContainsOndimS tag t

instance ContainsType (OndimS tag t) (MultiOndimS tag) =>
  ContainsOndimS tag t

withOndimGS :: (OndimTag tag) =>
  (OndimGS tag -> OndimGS tag) ->
  Ondim tag a ->
  Ondim tag a
withOndimGS f = Ondim . withMultiStateT f . unOndimT
{-# INLINABLE withOndimGS #-}

inhibitingExpansions :: OndimTag tag => Ondim tag a -> Ondim tag a
inhibitingExpansions = withOndimGS (\s -> s { inhibitExpansion = True })

withOndimS :: forall t tag a. (OndimTag tag, ContainsOndimS tag t) =>
  (OndimS tag t -> OndimS tag t) ->
  Ondim tag a ->
  Ondim tag a
withOndimS f = Ondim . withMultiStateT f . unOndimT

getExpansion :: forall t tag.
  OndimNode tag t => Text -> Ondim tag (Maybe (Expansion tag t))
getExpansion name = do
  gst <- Ondim $ mGet @(OndimGS tag)
  st  <- Ondim $ mGet @(OndimS tag t)
  if | Just fT <- fromText @tag,
       Just text <- lookup name (textExpansions gst) ->
         let aT = fromMaybe (const "") (asText @tag)
          in pure $ Just (expCtx name . fmap (foldMap fT) . text . fmap aT)
     | Just expansion <- lookup name (expansions st) ->
         pure $ Just (expCtx name . expansion)
     | otherwise -> pure Nothing
{-# INLINABLE getExpansion #-}

getTextExpansion ::
  OndimTag tag =>
  Text -> Ondim tag (Maybe (Expansion tag Text))
getTextExpansion k =
  expCtx k
    (Ondim $ mGets \s -> lookup k (textExpansions s))

liftSub ::
  forall tag t s.
  ( HasSub tag t s
  , OndimNode tag s
  , ContainsOndimS tag s
  , LiftAllSub (ExpTypes s)
  ) =>
  t -> Ondim tag t
liftSub node =
  let child = liftNodes (getSubs @tag @t @s node)
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
  forall tag t. OndimNode tag t =>
  t -> Ondim tag [t]
liftNode node = do
  gst <- Ondim $ mGet @(OndimGS tag)
  if inhibitExpansion gst
    then pure (one node)
    else case identify @tag node of
      Just name ->
        getExpansion name >>= \case
          Just expansion -> expansion liftedNode
          Nothing
            | Just valid <- validIdentifiers @tag @t,
              name `notElem` valid ->
                throwNotBound @t name
            | otherwise -> one <$> liftedNode
      _ -> one <$> liftedNode
  where
    liftedNode = liftSubstructures node
{-# INLINABLE liftNode #-}

withDebugCtx ::
  forall tag a.
  OndimTag tag =>
  (Int -> Int) ->
  ([Text] -> [Text]) ->
  Ondim tag a -> Ondim tag a
withDebugCtx f g (Ondim comp) =
  Ondim $ MultiStateT $ StateT \s -> do
    let gs :: OndimGS tag = getHListElem s
        depth' = expansionDepth gs
        trace' = expansionTrace gs
        gs' = gs { expansionDepth = f depth', expansionTrace = g trace' }
        s' = setHListElem gs' s
    (out, s'') <- runMultiStateT s' comp
    let gs'' :: OndimGS tag = getHListElem s''
        gs''' = gs'' { expansionDepth = depth', expansionTrace = trace' }
        s''' = setHListElem gs''' s''
    pure (out, s''')

expCtx :: forall tag a. OndimTag tag => Text -> Ondim tag a -> Ondim tag a
expCtx name ctx = do
  gst <- Ondim $ mGet @(OndimGS tag)
  if expansionDepth gst >= 200
    then -- To avoid recursive expansions
      throwError (MaxExpansionDepthExceeded $ expansionTrace gst)
    else withDebugCtx (+ 1) (name :) ctx

-- | Lift a list of nodes, applying filters.
liftNodes ::
  forall tag t. OndimNode tag t =>
  [t] -> Ondim tag [t]
liftNodes nodes = do
  st <- Ondim $ mGet @(OndimS tag t)
  foldr (.) id (filters st) $
    foldMapM (liftNode @tag) nodes

-- | Lift only the substructures of a node.
liftSubstructures :: forall tag t. OndimNode tag t => t -> Ondim tag t
liftSubstructures = liftAllSub @(ExpTypes t)
{-# INLINABLE liftSubstructures #-}

-- | "Bind" new expansions locally.
withExpansions :: OndimNode tag t => Expansions tag t -> Ondim tag a -> Ondim tag a
withExpansions exps =
  withOndimGS (\s -> s {textExpansions = foldr delete (textExpansions s) names}) .
  withOndimS (\s -> s {expansions = exps <> expansions s})
  where names = keys exps

-- | "Bind" filters locally.
withFilters :: OndimNode tag t => Filters tag t -> Ondim tag a -> Ondim tag a
withFilters filt = withOndimS (\s -> s {filters = filt <> filters s})

-- | "Bind" text expansions locally.
withText :: OndimTag tag => Expansions tag Text -> Ondim tag a -> Ondim tag a
withText exps = withOndimGS (\s -> s {textExpansions = exps <> textExpansions s})

-- | "Unbind" an expansion locally.
withoutExpansion :: forall t tag a. OndimNode tag t => Text -> Ondim tag a -> Ondim tag a
withoutExpansion name = withOndimS @t (\s -> s {expansions = delete name (expansions s)})

-- | "Unbind" a filter locally.
withoutFilter :: forall t tag a. OndimNode tag t => Text -> Ondim tag a -> Ondim tag a
withoutFilter name = withOndimS @t (\s -> s {filters = delete name (filters s)})

-- | "Unbind" a text expansion locally.
withoutText :: OndimTag tag => Text -> Ondim tag a -> Ondim tag a
withoutText name = withOndimGS (\s -> s {textExpansions = delete name (textExpansions s)})

-- | Put a new expansion into the local state, modifying the scope.
putExpansion :: OndimNode tag t => Text -> Expansion tag t -> Ondim tag ()
putExpansion key exps =
  Ondim $ mModify (\s -> s {expansions = insert key exps (expansions s)})

-- | Put a new expansion into the local state, modifying the scope.
putTextExp :: OndimTag tag => Text -> Expansion tag Text -> Ondim tag ()
putTextExp key exps =
  Ondim $ mModify (\s -> s {textExpansions = insert key exps (textExpansions s)})

type Expansions' m t = MapSyntax Text (Expansion m t)
type Filters' m t = MapSyntax Text (Filter m t)

-- | Convenience function to bind using MapSyntax.
binding :: OndimNode tag t =>
  Ondim tag a -> Expansions' tag t -> Ondim tag a
binding o exps = withExpansions (fromRight mempty (runMap exps)) o

-- | Convenience function to bind using MapSyntax.
bindingFilters :: OndimNode tag t =>
  Ondim tag a -> Filters' tag t -> Ondim tag a
bindingFilters o filts = withFilters (fromRight mempty (runMap filts)) o

-- | Convenience function to bind using MapSyntax.
bindingText :: OndimTag tag =>
  Ondim tag a -> Expansions' tag Text -> Ondim tag a
bindingText o exps = withText (fromRight mempty (runMap exps)) o

fromTemplate :: forall tag t.
  (OndimNode tag t, LiftAllSub (ExpTypes t), HasSub tag t t) =>
  Text -> [t] -> Expansion tag t
fromTemplate name tpl inner =
  liftNodes tpl `binding` do
    name <> ":content" ## const (children inner)

-- | Either applies expansion 'name', or throws an error if it does not exist.
callExpansion :: forall t tag. OndimNode tag t => Text -> Expansion tag t
callExpansion name arg = do
  exps <- getExpansion name
  maybe (throwNotBound @t name) ($ arg) exps

callText :: OndimTag tag => Text -> Expansion tag Text
callText k arg = do
  exps <- getTextExpansion k
  maybe (throwNotBound @Text k) ($ arg) exps
