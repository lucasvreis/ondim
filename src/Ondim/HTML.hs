{-# OPTIONS_GHC -Wno-orphans #-}
-- |

module Ondim.HTML where
import Ondim
import Text.XmlHtml qualified as X
import Relude.Extra.Lens

type OndimHtmlT m a = OndimT X.Node m a

instance OndimNode X.Node where
  identify (X.Element name _ _) = Just name
  identify _ = Nothing
  children = lens X.childNodes setNode
    where
      setNode el@X.Element{} c = el { X.elementChildren = c }
      setNode n _ = n
  attributes = lens getAttr setAttr
    where
      getAttr (X.Element _ a _) = a
      getAttr _ = []
      setAttr el@X.Element{} a = el { X.elementAttrs = a }
      setAttr n _ = n
  asText = X.nodeText
  injText = X.TextNode

-- | A hack, unfortunately. I could not find a single HTML Haskell library
-- properly supporting raw content.
rawNode :: Text -> X.Node
rawNode txt = X.Element "TO-BE-REMOVED" [("xmlhtmlRaw", "")] [X.TextNode txt]

-- * Template loading helpers

fromDocument :: Monad m => X.Document -> Expansion m X.Node
fromDocument doc = fromTemplate $ X.docContent doc

expandDocument :: Monad m => X.Document -> OndimHtmlT m X.Document
expandDocument doc = do
  nodes <- foldMapM liftNode (X.docContent doc)
  pure $ doc { X.docContent = nodes }
