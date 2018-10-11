{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Computation where


import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Error
import AbsHaml


type Env = M.Map Name (Computation Value)
type Scope = Env

data Value = StringVal String
           | IntVal Integer
           | BoolVal Bool
           | ListVal [Value]
           | FunVal Param FunBody Scope

data FunBody = ExpBody Exp
             | CompBody (Computation Value) -- only used for builtins

newtype Computation a = Computation { runComputation :: ReaderT Env (ErrorT String IO) a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError String)

instance Show Value where
        show (StringVal s) = show s
        show (IntVal i) = show i
        show (BoolVal b) = show b
        show (ListVal l) = show l
        show (FunVal _ _ _) = "a function"

instance Eq Value where
        (StringVal a) == (StringVal b) = a == b
        (IntVal a) == (IntVal b) = a == b
        (BoolVal a) == (BoolVal b) = a == b
        (ListVal a) == (ListVal b) =
            length a == length b && and (zipWith (==) a b)
        _ == _ = False

initialEnv :: Env
initialEnv = makeEnv [("head", ["list"], headBody)
                     ,("tail", ["list"], tailBody)
                     ,("empty", ["list"], emptyBody)]

                     [(IOCons, ["head", "tail"], consBody)
                     ,arithmeticOp IOPlus ((+) :: (Integer -> Integer -> Integer))
                     ,arithmeticOp IOMinus (-)
                     ,arithmeticOp IOMul (*)
                     ,(IODiv, ["a", "b"], divBody)
                     ,compareOp IOLt (<)
                     ,compareOp IOGt (>)
                     ,compareOp IOLte (<=)
                     ,compareOp IOGte (>=)
                     ,(IOEq, ["a", "b"], eqBody)]

    where
        headBody = do
            listComp <- getParam "list"
            case listComp of (ListVal list) -> return $ head list
                             _ -> throwError "type error: cannot take head of a non-list"
        tailBody = do
            listComp <- getParam "list"
            case listComp of (ListVal list) -> return $ ListVal $ tail list
                             _ -> throwError "type error: cannot take tail of a non-list"
        emptyBody = do
            listComp <- getParam "list"
            case listComp of (ListVal list) -> return $ BoolVal $ null list
                             _ -> throwError "type error: empty only works with lists"
        consBody = do
            hd <- getParam "head"
            tailComp <- getParam "tail"
            case tailComp of (ListVal tl) -> return $ ListVal $ hd:tl
                             _ -> throwError "type error: the second argument to cons must be a list"
        eqBody = liftM2 (\a b -> BoolVal $ a == b) (getParam "a") (getParam "b")
        divBody = do
            intCompA <- getParam "a"
            intCompB <- getParam "b"
            case (intCompA, intCompB) of
                (IntVal _, IntVal 0) -> throwError "error: division by zero"
                (IntVal a, IntVal b) -> return $ IntVal $ a `div` b
                _ -> throwError "type error in arithmetic operation: can only be used with numbers"

        arithmeticOp op fun = (op, ["a", "b"], arithOpBody fun)
        compareOp op fun = (op, ["a", "b"], compareOpBody fun)

        arithOpBody fun = do
            intCompA <- getParam "a"
            intCompB <- getParam "b"
            case (intCompA, intCompB) of
                (IntVal a, IntVal b) -> return $ IntVal $ a `fun` b
                _ -> throwError "type error in arithmetic operation: can only be used with numbers"
        compareOpBody fun = do
            intCompA <- getParam "a"
            intCompB <- getParam "b"
            case (intCompA, intCompB) of
                (IntVal a, IntVal b) -> return $ BoolVal $ a `fun` b
                _ -> throwError "type error in comparison operation: can only be used with numbers"

        makeEnv funs ops = M.fromList $ (map makeBuiltinFun funs) ++ (map makeBuiltinOp ops)
        makeBuiltinFun (name, pars, body) = (NIdent (Ident name), makeFunVal pars body)
        makeBuiltinOp (op, pars, body) = (NOp op, makeFunVal pars body)
        makeFunVal pars body =
            case pars of [] -> body
                         p:ps -> do
                             env <- ask
                             return $ FunVal (PIdent (Ident p))
                                             (CompBody (makeFunVal ps body))
                                             env
        getParam str = do
            (Just param) <- asks (M.lookup (NIdent (Ident str)))
            param
