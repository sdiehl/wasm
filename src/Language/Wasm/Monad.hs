module Language.Wasm.Monad where

import Control.Monad (ap)
{-import qualified Data.Char as Char-}
{-import Data.Char (isAscii)-}
{-import Data.Word (Word8)-}

import Language.Wasm.Lexer

-- | Parser monad state
data S = S [String] [Token]

data ParseError
  = ParseError String
  | ParseErrorEOF
  deriving Show

-- | Parsing monad
newtype ParseM a
  = ParseM { unParseM :: S -> Either ParseError (a, S) }

runParseM :: ParseM a -> [Token] -> Either ParseError a
runParseM m ts = fst <$> unParseM m (S cs ts')
  where (cs, ts') = splitCommentTokens ts

instance Functor ParseM where
  fmap f m = ParseM $ \s ->
    case unParseM m s of
      Left err      -> Left err
      Right (x, s') -> Right (f x, s')

instance Monad ParseM where
  return x = ParseM $ \s -> Right (x, s)
  m >>= k  = ParseM $ \s ->
    case unParseM m s of
      Left err      -> Left err
      Right (x, s') -> unParseM (k x) s'

instance Applicative ParseM where
  pure = return
  (<*>) = ap

splitCommentTokens :: [Token] -> ([String], [Token])
splitCommentTokens = go []
  where
    go cs (TComment c : ts) = go (c : cs) ts
    go cs ts                = (reverse cs, ts)

-- | Return the list of comment tokens at the current location
getComments :: ParseM [String]
getComments = ParseM $ \(S cs ts) -> Right (cs, S [] ts)

-- | Return the next non-comment token
nextNonCommentToken :: ParseM Token
nextNonCommentToken = ParseM $ \(S _ ts) ->
  let (cs, ts') = splitCommentTokens ts in
  case ts' of
    (t : ts'') -> Right (t, S cs ts'')
    []         -> Right (TEnd, S cs ts')

lexerP :: (Token -> ParseM a) -> ParseM a
lexerP = (nextNonCommentToken >>=)
