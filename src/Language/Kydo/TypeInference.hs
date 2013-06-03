{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TupleSections   #-}
module Language.Kydo.TypeInference where
import Language.Kydo.Syntax
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Control.Lens
import Control.Lens.Setter
import Control.Lens.Iso
import Data.Default
import Data.Maybe
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Language.Kydo.QuasiQuoter
import Text.PrettyPrint.Free

type Subst = Map Sym Type

data Scheme = Scheme 
   { vars :: [Sym] 
   , typ  :: Type 
   }

class Types a where
   ftv   :: a -> Set Sym
   apply :: Subst -> a -> a

instance Types Type where
   ftv = \case 
      TVar x   -> S.singleton x
      TInt     -> S.empty
      TFun x y -> ftv x `S.union` ftv y
   
   apply s = \case 
      TVar n   -> fromMaybe (TVar n) $ M.lookup n s
      TFun x y -> TFun (apply s x) (apply s y)
      x        -> x

instance Types Scheme where
   ftv (Scheme vars t) = ftv t S.\\ S.fromList vars
   apply s (Scheme vars t) = Scheme vars . apply (foldr M.delete s vars) $ t

instance Types a => Types [a] where
   apply s = map (apply s)
   ftv l   = foldr S.union S.empty (map ftv l)

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst x y = M.map (apply x) y `M.union` x

newtype TypeEnv = TypeEnv { unTypeEvn :: Map String Scheme}

instance Default TypeEnv where
   def = TypeEnv M.empty

makeIso ''TypeEnv
 
remove :: String -> TypeEnv -> TypeEnv
remove var env = under typeEnv (M.delete var) env

instance Types TypeEnv where
   ftv (TypeEnv env)     = ftv . M.elems $ env
   apply s = under typeEnv (M.map (apply s))

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme (S.toList $ ftv t S.\\ ftv env) t

newtype TIEnv = TIEnv { unTIEnv :: Map Sym Scheme }

instance Default TIEnv where
   def = TIEnv M.empty

makeIso ''TIEnv

data TIState = TIState
   { _supply :: Int
   , _subst  :: Subst
   }
   deriving (Show, Eq, Read, Ord)

instance Default TIState where
   def = TIState 0 M.empty

makeLenses ''TIState

data TypeError
   = UnificationError Type Type
   | OccursCheck String Type
   | UnboundVar Sym
   | UnknownError String
   deriving (Show, Eq, Read, Ord)
   
instance Error TypeError where
   strMsg = UnknownError

type TI m a = ErrorT TypeError (ReaderT TIEnv (StateT TIState m)) a

runTI :: Monad m => TI m a -> m (Either TypeError a, TIState)
runTI t = runStateT (runReaderT (runErrorT t) def) def where

postInc = do
   supply += 1
   use supply

newTyVar :: Monad m => String -> TI m Type
newTyVar prefix = do
   s <- postInc
   return . TVar $ prefix ++ show s
   
arbVar = newTyVar "a"

instanciate :: Monad m => Scheme -> TI m Type
instanciate (Scheme vars t) = do
   nvars <- mapM (const arbVar) vars
   let s = M.fromList $ zip vars nvars 
   return $ apply s t

mgu :: Monad m => Type -> Type -> TI m Subst 
mgu x y = case (x, y) of
   (TFun l r, TFun l' r') -> do 
      s1 <- mgu l l'
      s2 <- mgu (apply s1 r) (apply s1 r')
      return $ s1 `composeSubst` s2
   (TVar u, t)  -> varBind u t
   (t, TVar u)  -> varBind u t
   (TInt, TInt) -> return nullSubst
   _ -> throwError $ UnificationError x y
   
varBind :: Monad m => String -> Type -> TI m Subst
varBind u t 
   | t == TVar u        = return nullSubst
   | u `S.member` ftv t = throwError $ OccursCheck u t
   | otherwise          = return $ M.singleton u t
   
tlookup :: Sym -> TypeEnv -> Maybe Scheme
tlookup n te = M.lookup n . view (from typeEnv) $ te

ti :: Monad m => TypeEnv -> Expr -> TI m (Subst, Type)
ti env v = case v of
   Var n -> maybe (throwError . UnboundVar $ n)  
      (liftM (nullSubst,) . instanciate) . tlookup n $ env
   Lit l   -> return (nullSubst, TInt)
   Abs n e -> do
      tv <- arbVar
      let env'  = remove n env
          env'' = under typeEnv (flip M.union $ M.singleton n (Scheme [] tv)) env'
      (x, y) <- ti env'' e
      return $ (x, TFun (apply x tv) y)
   App e1 e2 -> do
      tv <- arbVar
      (s1, t1) <- ti env e1
      (s2, t2) <- ti (apply s1 env) e2
      s3       <- mgu (apply s2 t1) (TFun t2 tv)
      return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
   Let x e1 e2 -> do
      (s1, t1) <- ti env e1
      let TypeEnv env' = remove x env
          t'    = generalize (apply s1 env) t1
          env'' = TypeEnv $ M.insert x t' env'
      (s2, t2) <- ti (apply s1 env'') e2
      return (s1 `composeSubst` s2, t2)
          
typeInference :: Monad m => Map String Scheme -> Expr -> TI m Type
typeInference env e = do
   (s, t) <- ti (TypeEnv env) e
   return (apply s t)


e0 = [kydo| let id = \x -> x in id    |]
e1 = [kydo| let id = \x -> x in id id |]
e2 = [kydo| let id = (\x -> let y = x in y) in id id |]
e3 = [kydo| let id = (\x -> let y = x in y) in id id 2 |]
e4 = [kydo| let id = \x -> x x in id |]


test :: Expr -> IO () 
test e = do 
   (res ,_) <- runTI (typeInference M.empty e ) 
   case res of
      Left err -> putStrLn $ "error: " ++ show err
      Right t  -> putStrLn $ show e ++ " :: " ++ show t

main :: IO ()
main = mapM_ test [e0, e1, e2, e3, e4]




