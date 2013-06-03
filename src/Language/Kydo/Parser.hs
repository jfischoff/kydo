{-# LANGUAGE FlexibleContexts #-}
module Language.Kydo.Parser where
import Text.Parser
import Language.Kydo.Syntax
import Control.Applicative
import Control.Monad
import Control.Monad.Error
--import Text.Parsec hiding ((<|>), digit, optional, parse, many)
import Data.Char

type SParser a = Parser Char a

isReserved x = elem x ["in", "let"]

pIdentifier :: SParser String
pIdentifier = try $ do 
   ident <- (:) <$> lowerCaseLetter <*> many alpha
   if isReserved ident 
      then throwError . strMsg $ ident ++ " is a reserved keyword"
      else return ident

definitelySpace :: SParser String
definitelySpace = some whitespace

digit = satisfy isDigit

natural = read <$> some digit

integer :: SParser Integer
integer = do
   sign <- maybe 1 (const (-1)) <$> optional (sym '-')
   spaces 
   n <- natural
   return $ sign * n

pExpr :: SParser Expr
pExpr = parens pExpr <|> do 
   spaces
   result <- pApp <|> pAtom
   spaces
   return result
     
pAtom :: SParser Expr    
pAtom =  pLit
     <|> pLet
     <|> pVar
     <|> pAbs

pVar :: SParser Expr
pVar =  Var 
    <$> pIdentifier

pLit :: SParser Expr
pLit =  Lit 
    <$> integer

pApp :: SParser Expr
pApp = chainl1 pAtom (definitelySpace >> return App) 

pAbs :: SParser Expr
pAbs =  Abs
    <$> (sym '\\' >> pIdentifier)
    <*> (spaces >> string "->" >> spaces >> pExpr)

pLet :: SParser Expr
pLet =  Let
    <$> (string "let" >> spaces >> pIdentifier)
    <*> (spaces >> string "="   >> spaces >> pExpr)
    <*> (spaces >> string "in"  >> spaces >> pExpr)

class Parsable a where
   parse :: String -> Either (ParseError Char) a

instance Parsable Expr where
   parse = runParser pExpr
   
parseExpr :: String -> Either (ParseError Char) Expr
parseExpr = parse


