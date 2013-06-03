{-# LANGUAGE LambdaCase #-}
module Language.Kydo.Syntax where
import Text.PrettyPrint.Free

type Sym = String

data Expr 
   = Var Sym
   | Lit Integer
   | App Expr Expr
   | Abs Sym Expr
   | Let Sym Expr Expr
   deriving (Eq, Show, Read, Ord)

data Type 
   = TVar Sym
   | TInt
   | TFun Type Type
   deriving (Eq, Show, Read, Ord)
   
instance Pretty Expr where
   pretty = \case 
      Var x     -> pretty x
      Lit x     -> pretty x
      App x y   -> pretty x <+> pretty y
      Abs n e   -> text "\"" <> pretty n <+> text "->" <+> pretty e
      Let n x y -> text "let" <+> pretty n <+> text "=" <+> pretty x 
                     <+> text "in" <+> pretty y