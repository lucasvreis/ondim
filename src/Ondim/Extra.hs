-- | Examples of expansions.

module Ondim.Extra where
import Ondim
import Data.Map.Syntax
import Relude.Extra.Lens
import Data.List qualified as L

defaultExpansions :: (Monad m, OndimNode t) => Expansions m t
defaultExpansions =
  fromRight mempty $ runMap $ do
    "ignore" ## ignore
    "switch" ## switchBound
    "if-bound" ## ifBound
    "bind" ## bind

ignore :: Monad m => a -> OndimT t m [b]
ignore = const $ pure []

ifElse :: (Monad m, OndimNode t) => Bool -> Expansion m t
ifElse cond oel = do
  els <- view children <$> oel
  let (ifEls, drop 1 -> elseEls) =
        break ((Just "else" ==) . identify) els
  if cond
    then pure ifEls
    else pure elseEls

switchCases :: (Monad m, OndimNode t) => Text -> Expansions' m t
switchCases tag = do
  "case" ## \caseNode -> do
    attrs <- view attributes <$> caseNode
    if isJust (L.lookup tag attrs) || Just tag == L.lookup "tag" attrs
    then view children <$> caseNode
    else pure []

switch :: (Monad m, OndimNode t) =>
  Text -> Expansion m t
switch tag = runChildrenWith (switchCases tag)

switchWithDefault :: (Monad m, OndimNode t) =>
  Text -> Expansion m t
switchWithDefault tag oel = do
  els <- view children <$> oel
  pure $ fromMaybe [] $ fmap (view children) $
    find (\x -> nameIs "case" x && hasTag x) els <|>
    find (\x -> nameIs "default" x) els
  where
    nameIs n x = identify x == Just n
    hasTag (view attributes -> attrs) =
      isJust (L.lookup tag attrs) ||
      Just tag == L.lookup "tag" attrs


-- Implementations

ifBound :: (Monad m, OndimNode t) => Expansion m t
ifBound oel = do
  node <- oel
  let attrs = node ^. attributes
  bound <- case L.lookup "tag" attrs of
    Just tag -> isJust <$> getExpansion tag
    Nothing -> pure False
  ifElse bound (pure node)

switchBound :: (Monad m, OndimNode t) => Expansion m t
switchBound oel = do
  node@(view children -> els) <- oel
  let attrs = node ^. attributes
  fromMaybe els <$> runMaybeT do
    tag <- hoistMaybe $ L.lookup "tag" attrs
    tagC <- lift (foldMap asText <$> callExpansion tag)
    lift $ switchWithDefault tagC oel

-- | This expansion works like Heist's `bind` splice
bind :: (Monad m, OndimNode t) => Expansion m t
bind node = do
  attrs <- view attributes <$> node
  whenJust (L.lookup "tag" attrs) $ \tag ->
    putExpansion tag $ \inner ->
      view children <$> node
      `bindingExpansions` do
        "apply-content" ## const (view children <$> inner)
  pure []
