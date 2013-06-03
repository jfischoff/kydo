{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Parser where
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Error
import Control.Lens
import Control.Applicative
import Data.Char
import Data.Functor.Identity

data ParseError s
   = UserError String
   | UnexpectedEndOfInput
   | UnexpectedInput s s
   | Death
   | FailedToSatisfy s
   deriving (Show, Eq, Read, Ord)
   
instance Error (ParseError s) where
   strMsg = UserError
   noMsg  = Death

data ParserState stream = ParserState 
   { _stream :: stream
   , _index  :: Int
   }

makeParserState s = ParserState s 0

makeLenses ''ParserState

-- TODO add reader for a stack
newtype ParserT s m a = 
   ParserT { unParserT :: StateT (ParserState [s]) (ErrorT (ParseError s) m) a }
      deriving (MonadError (ParseError s), MonadState (ParserState [s]), 
                  Monad, Functor, Applicative, Alternative, MonadPlus)

makeIso ''ParserT

type Parser s = ParserT s Identity

incToken :: (Monad m) => ParserT s m s
incToken = do 
   l <- uses stream length
   i <- use index
   mresult <- use $ pre (stream.ix i)
   case mresult of
      Just x  -> index += 1 >> return x
      Nothing -> throwError UnexpectedEndOfInput

satisfy :: Monad m => (s -> Bool) -> ParserT s m s
satisfy f = do
   x <- incToken
   if f x  
      then return x
      else throwError $ FailedToSatisfy x

sym :: (Eq s, Monad m) => s -> ParserT s m s
sym a = do 
   x <- incToken
   if x == a 
      then return x
      else throwError $ UnexpectedInput a x

try :: Monad m => ParserT s m a -> ParserT s m a
try p = do
   s <- get 
   p `catchError` (\e -> put s >> throwError e)

choice :: (Eq s, Monad m, Functor m) => [ParserT s m s] -> ParserT s m s
choice = foldr (<|>) empty

infixl 3 `opt`

opt :: (Monad m, Functor m) => ParserT s m a -> a -> ParserT s m a
p `opt` v = p <|> return v

(<??>) :: (Monad m, Functor m) 
       => ParserT s m a -> ParserT s m (a -> a) -> ParserT s m a
p <??> q = p <**> (q `opt` id)

chainr :: (Functor m, Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainr p op = r where r = p <??> (flip <$> op <*> r)

chainl :: (Functor m, Monad m) 
       => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainl p op = applyAll <$> p <*> many (flip <$> op <*> p)

chainl1 :: (Functor m, Monad m) 
       => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainl1 p op = applyAll <$> p <*> some (flip <$> op <*> p)

applyAll :: a -> [a -> a] -> a
applyAll x (f:fs) = applyAll (f x) fs
applyAll x [] = x

string :: (Eq s, Functor m, Monad m) => [s] -> ParserT s m [s]
string = \case
   x:xs -> (:) <$> sym x <*> string xs
   []   -> return []

runParserT :: Monad m => ParserT s m a -> [s] -> m (Either (ParseError s) a)
runParserT p s = runErrorT (evalStateT (unParserT p) $ makeParserState s)

runParser :: Parser s a -> [s] -> Either (ParseError s) a
runParser p = runIdentity . runParserT p

---------- Char specific --------------------

parens :: (Functor m, Monad m) => ParserT Char m a -> ParserT Char m a
parens p = id <$ sym '(' <*> p <* sym ')'

lowerCaseLetter = satisfy (\x -> isAlpha x && isLower x)  

alpha = satisfy isAlpha

whitespace = satisfy isSpace

spaces = many whitespace














