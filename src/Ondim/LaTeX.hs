module Ondim.LaTeX where

import Data.Char (toLower, isUpper)
import Data.Text qualified as T
import Ondim
import Text.LaTeX.Base.Syntax qualified as L

-- | Ondim LaTeX tag. (Used for instances).
data LaTeXTag (m :: Type -> Type)

-- | We use a new LaTeX datatype because the one provided by HaTeX is not
--   structured the way we want.
data TeXNode
  = TeXEnv {envName :: Text, envArgs :: [TeXArg], envContent :: [TeXNode]}
  | TeXComm {commName :: Text, commArgs :: [TeXArg]}
  | TeXBraces [TeXNode]
  | TeXMath L.MathType [TeXNode]
  | TeXLineBreak (Maybe L.Measure) Bool
  | TeXRaw Text
  deriving (Eq, Show, Generic)

data TeXArg
  = FixArg [TeXNode]
  | OptArg [TeXNode]
  deriving (Eq, Show, Generic)

fromHaTeX :: L.LaTeX -> [TeXNode]
fromHaTeX = go False
  where
    go strip = \case
      (L.TeXRaw t)
        | T.null t -> []
        | strip -> [TeXRaw $ T.stripStart t]
        | otherwise -> [TeXRaw t]
      L.TeXComm n a -> [TeXComm (toText n) (fromHArgs a)]
      L.TeXCommS n -> [TeXComm (toText n) []]
      L.TeXEnv n a c -> [TeXEnv (toText n) (fromHArgs a) (go False c)]
      L.TeXMath t c -> [TeXMath t (go False c)]
      L.TeXLineBreak m b -> [TeXLineBreak m b]
      L.TeXBraces c -> [TeXBraces (go False c)]
      L.TeXComment {} -> []
      L.TeXSeq (L.TeXComment {}) y -> go True y
      L.TeXSeq x y -> go strip x <> go False y
      L.TeXEmpty {} -> []
    fromHArgs :: [L.TeXArg] -> [TeXArg]
    fromHArgs = foldr go' []
      where
        go' (L.FixArg a) = (FixArg (fromHaTeX a) :)
        go' (L.OptArg a) = (OptArg (fromHaTeX a) :)
        go' _ = id

toHaTeX :: [TeXNode] -> L.LaTeX
toHaTeX = foldMap go
  where
    go = \case
      TeXRaw t -> L.TeXRaw t
      TeXComm n a -> L.TeXComm (toString n) (toHArgs a)
      TeXEnv n a c -> L.TeXEnv (toString n) (toHArgs a) (toHaTeX c)
      TeXMath t c -> L.TeXMath t (toHaTeX c)
      TeXLineBreak m b -> L.TeXLineBreak m b
      TeXBraces c -> L.TeXBraces (toHaTeX c)
    toHArgs :: [TeXArg] -> [L.TeXArg]
    toHArgs = map go'
      where
        go' (FixArg a) = L.FixArg (toHaTeX a)
        go' (OptArg a) = L.OptArg (toHaTeX a)

instance Monad m => OndimTag (LaTeXTag m) where
  type OndimTypes (LaTeXTag m) = '[L.LaTeX, TeXNode, TeXArg, Text]
  type OndimMonad (LaTeXTag m) = m

instance Monad m => OndimNode (LaTeXTag m) L.LaTeX where
  type ExpTypes L.LaTeX = '[TeXNode]

instance HasSub (LaTeXTag m) L.LaTeX TeXNode where
  getSubs = fromHaTeX
  setSubs _ l = toHaTeX l

instance Monad m => OndimNode (LaTeXTag m) TeXNode where
  type ExpTypes TeXNode = '[TeXNode, TeXArg]
  identify = \case
    (TeXEnv {envName = n})
      | Just n' <- stripAndCase n -> Just n'
    (TeXComm {commName = n})
      | Just n' <- stripAndCase n -> Just n'
    _ -> Nothing
    where
      stripAndCase t = do
        s <- T.stripPrefix "exp" t
        (c, s') <- T.uncons s
        guard (isUpper c)
        return $ T.cons (toLower c) s'
  fromText = Just (one . TeXRaw)
  validIdentifiers = Just []

instance HasSub (LaTeXTag m) TeXNode TeXNode

instance HasSub (LaTeXTag m) TeXNode TeXArg

instance Monad m => OndimNode (LaTeXTag m) TeXArg where
  type ExpTypes TeXArg = '[TeXNode]

instance HasSub (LaTeXTag m) TeXArg TeXNode
