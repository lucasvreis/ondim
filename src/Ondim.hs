{- | This module exports the main definitions and methods for working with Ondim.

Here, we shall call by a "node" any type that implements 'OndimNode', which
means it is compatible with the Ondim template system. Many types can be used as
nodes at once, and in fact in a single format like HTML there are multiple types
that are regarded as nodes (the HTML document, element, node and attribute are
different types implementing the class). In other formats like Pandoc the
necessity of this approach is more evident, since there are not only the @Block@
and @Inline@ types but also metadata types and so on.

Each node may have an /name/, which is simply a @Text@ string and acessed by the
'identify' class method. A node which has an name is meant to be /expanded/ by
the template system, and this is done by applying it to an 'Expansion' bound to
the same name in the local state.

Another thing specified by the 'OndimNode' instances is how different node types
are found inside a given node type, and this information is used to recursively
expand the templates in a macro-like fashion. The main sorts of "substructures"
of a node are its /children/ and /attributes/, which are acessed via the
'children' and 'attributes' methods, repectively. Since those are part of the
class, they can be used for writing expansions that don't depend on the node
type. Those polymorphic expansions are special enough to have a type alias for
them: 'PolyExpansion'. The nice thing about them is that you can write
templating code only once and expand templates of any format, while still being
able to specify expansions for a given type.

So the main function in this module is probably 'expandNode', which when applied
to a node runs the expansion machinery and turns the node into something else.

Expansions are just Haskell functions of type @t -> 'Ondim' s [t]@, and can be
defined inside Haskell code. The expansion's argument is the node whose name
matched the name of the expansion (the "caller"), and the expansion returns a
list of nodes that will replace it in the expansion process. Expansions are
bound to names in the 'Ondim' monad state, and they can also be organized into
more nested 'Namespace's. See "Ondim.State" for more information on how to
manipulate the state.

The state also store /templates/, which are essentialy values of type
@'OndimNode' t => t@ and are used to store data like text and file templates.
Every template stored in the state is also seen as an expansion, the expansion
that ignores the caller argument and returns the expanded template:

@
templateToExpansion tpl = const $ 'expandNode' tpl
@

This is not the true definition --- the caller node is not actually ignored,
instead it may be acessed from the template via the state in the @caller@
namespace. Most importantly, templates may be loaded from disk, see the module
"Ondim.Loading".

For dealing with exceptions and debug data, see the module "Ondim.Debug".
-}
module Ondim
  ( -- * Monad
    Ondim,
    evalOndimWith,
    evalOndim,
    liftST,

    -- * Nodes
    OndimNode,
    identify,
    ondimCast,

    -- * Children and attributes
    children,
    expandChildren,
    lookupAttr,
    attributes,

    -- * Running templates
    expandNode,
    expandSubs,

    -- * Data types
    Expansion,
    PolyExpansion,

    -- * State transformations
    module Ondim.State,

    -- ** Get specific expansions
    getExpansion,
    getTemplate,
    getNamespace,
    getText,

    -- ** Calling
    callExpansion,
    callTemplate,
    callText,

    -- * Rendering
    renderNode,
    renderNodeOrError,
    renderTemplateOrError,

    -- * Auxiliary
    Attribute,
  )
where

import Control.Monad.ST (ST)
import Data.List qualified as L
import Data.STRef (newSTRef)
import Ondim.Internal.Basic
import Ondim.Internal.Class
import Ondim.Internal.Core
import Ondim.State
import Prelude hiding (All)

-- | Runs the Ondim action with a given initial state.
evalOndimWith ::
  OndimState s ->
  Ondim s a ->
  ST s (Either OndimException a)
evalOndimWith s o = do
  ref <- newSTRef s
  unOndimT o
    & usingReaderT (initialTraceData, ref)
    & runExceptT

-- | Runs the Ondim action with empty initial state.
evalOndim :: Ondim s a -> ST s (Either OndimException a)
evalOndim = evalOndimWith mempty

-- Children

{- | Returns the children of a node after expanding them.

@
'expandChildren' = 'expandNodes' . 'children'
@
-}
expandChildren :: (OndimNode t) => Expansion s t
expandChildren = expandSubs . children

-- Attributes

-- | Lookup an attribute from a node by name.
lookupAttr :: (OndimNode t) => Text -> t -> Ondim s (Maybe Text)
lookupAttr key = fmap (L.lookup key) . attributes

-- | Render node as bytestring, if possible, or fail.
renderNodeOrError :: (HasCallStack) => (OndimNode a) => a -> Ondim s LByteString
renderNodeOrError =
  case renderNode of
    Just render -> return . render
    Nothing -> const $ throwTemplateError "This type cannot be rendered."

-- | Expand and then render template called 'name' to bytestring.
renderTemplateOrError :: (HasCallStack) => Text -> Ondim s LByteString
renderTemplateOrError name = do
  mbValue <- lookup name . expansions <$> getOndimS
  case mbValue of
    Just (TemplateData site thing) ->
      renderNodeOrError
        =<< withSite site (expandSubs thing)
    Just _ -> throwExpFailure @() name (FailureOther "Identifier not bound to a template.")
    Nothing -> throwExpFailure @() name NotBound

-- | Either applies template 'name', or throws a failure if it does not exist.
callTemplate :: forall t s. (OndimNode t) => Text -> Ondim s [t]
callTemplate name = do
  exps <- getTemplate name
  either (throwExpFailure @t name) return exps

-- | Either applies text 'name', or throws a failure if it does not exist.
callText :: Text -> Ondim s Text
callText name = do
  exps <- getText name
  either (throwExpFailure @Text name) return exps

-- | Either applies expansion 'name', or throws a failure if it does not exist.
callExpansion :: forall t s. (OndimNode t) => Text -> Expansion s t
callExpansion name arg = do
  exps <- getExpansion name
  either (throwExpFailure @t name) ($ arg) exps
