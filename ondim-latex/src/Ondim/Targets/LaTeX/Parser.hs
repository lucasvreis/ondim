-- | This is a fake LaTeX parser.
module Ondim.Targets.LaTeX.Parser where

import Data.Char (isAsciiLower, isAsciiUpper, isSpace, isUpper, toLower, isNumber)
import Data.Sequence (Seq (..), (|>))
import Data.Text qualified as T
import Ondim.Targets.LaTeX.Instances (LaTeXNode (..), LaTeXDoc (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many, some)

parseLaTeX :: String -> Text -> Either String LaTeXDoc
parseLaTeX fp =
  first errorBundlePretty
    . parse (runReaderT document initialParserstate) fp

document :: Parser LaTeXDoc
document = do
  Nodes nodes <- manyNodes
  eof
  return $ LaTeXDoc (toList nodes)

newtype Nodes = Nodes {unNodes :: Seq LaTeXNode}

instance Semigroup Nodes where
  (Nodes (xs :|> Text t1)) <> (Nodes (Text t2 :<| ys)) =
    Nodes ((xs |> Text (t1 <> t2)) <> ys)
  (Nodes xs) <> (Nodes ys) =
    Nodes (xs <> ys)

instance Monoid Nodes where
  mempty = Nodes mempty

instance One Nodes where
  type OneItem Nodes = LaTeXNode
  one = Nodes . one

newtype ParserState = ParserState
  { level :: Int
  }

initialParserstate :: ParserState
initialParserstate = ParserState 0

type Parser = ReaderT ParserState (Parsec Void Text)

isHSpace :: Char -> Bool
isHSpace c = isSpace c && c /= '\n'

manyNodes :: Parser Nodes
manyNodes =
  mconcat
    <$> many
      ( braces
          <|> lineEnd
          <|> command
          <|> escape
          <|> spaceEater
          <|> comment
          <|> prose
      )

command :: Parser Nodes
command = do
  _ <- try $ string "\\@"
  name <- takeWhileP Nothing isAllowedName
  attrs <- option [] do
    _ <- try do
      space
      char '['
    space
    pair `sepBy` char ',' <* char ']'
  arg <- option mempty do
    _ <- try do
      space
      char '{'
    inner <* char '}'
  return $ one (Command (unCamel name) attrs arg)
  where
    unCamel = T.concatMap \c ->
      if isUpper c
        then T.pack ['-', toLower c]
        else one c
    inner =
      local
        (\s -> s {level = 1 + level s})
        (toList . unNodes <$> manyNodes)
    isAllowedKey c = isAllowedName c || isNumber c || c == '.' || c == '-' || c == ':'
    gpVal = char '{' *> (unNodes <$> manyNodes) <* char '}'
    nVal = one . Text <$> takeWhile1P Nothing isAllowedKey
    pair = do
      space
      k <- try gpVal <|> nVal
      space
      v <- option mempty $ do
        char '=' *> space
        (try gpVal <|> nVal)
          <* space
      return (toList k, toList v)

lineEnd :: Parser Nodes
lineEnd = do
  _ <- char '\n'
  n <- asks level
  s <- takeWhileP Nothing isHSpace
  return $ one $ Text $ T.cons '\n' $ T.drop (2 * n) s

escape :: Parser Nodes
escape = try do
  _ <- char '\\'
  s <- satisfy \c -> not $ isAsciiLower c || isAsciiUpper c
  return $ one $ Text $ T.pack [s]

prose :: Parser Nodes
prose = do
  s <- satisfy \c -> c /= '\n' && c /= '}' && c /= '%'
  t <-
    takeWhileP
      Nothing
      (\c -> c /= '\\' && c /= '\n' && c /= '{' && c /= '}' && c /= '%')
  return $ Nodes (one $ Text $ T.cons s t)

comment :: Parser Nodes
comment = do
  _ <- try $ char '%'
  c <- takeWhileP Nothing (/= '\n')
  return $ Nodes (one $ Comment c)

braces :: Parser Nodes
braces = do
  _ <- try $ char '{'
  x <- manyNodes
  _ <- char '}'
  return $ one (Text "{") <> x <> one (Text "}")

spaceEater :: Parser Nodes
spaceEater = do
  _ <- try $ string "%!\n"
  space
  return mempty

isAllowedName :: Char -> Bool
isAllowedName c = isAsciiLower c || isAsciiUpper c || c == '@'
