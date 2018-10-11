module Interpreter where


import Computation

import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Error
import System.IO
import AbsHaml


execProgram :: Program -> IO ()
execProgram (Prog es) = do
        res <- runErrorT (runReaderT (runComputation (evalTopExps es)) initialEnv)
        case res of Left str -> hPutStrLn stderr str
                    Right _  -> return ()

evalTopExps :: [TopExp] -> Computation ()
evalTopExps [] = return ()
evalTopExps (x:xs) =
        let rest = evalTopExps xs in
        case x of
            TExp exp -> do
                val <- evalExp exp
                liftIO $ print val
                rest
            TLet lbind -> do
                modifyEnv <- evalLetBinding lbind
                local modifyEnv rest

evalExp :: Exp -> Computation Value
evalExp (ELet binds exp) = do
        modifyEnv <- liftM (foldl (.) id) (mapM evalLetBinding binds)
        local modifyEnv (evalExp exp)

evalExp (ELam param1 params exp) =
        case params of
            -- (\x1 x2.. -> e) == (\x1 -> \x2 -> .. -> e)
            p:ps -> evalExp $ ELam param1 [] (ELam p ps exp)
            []   -> do
                env <- ask
                return $ FunVal param1 (ExpBody exp) env

evalExp (EIf cond whatThen whatElse) = do
        boolComp <- evalExp cond
        case boolComp of
            (BoolVal valCond) -> evalExp $ if valCond then whatThen else whatElse
            _ -> throwError "type error: an if statement's condition must be a boolean"

evalExp (EIOp leftExp op rightExp) =
        evalExp $ ECall (EName (NOp op)) [leftExp, rightExp]

evalExp (EUOp op exp) =
        case op of UOMinus -> do
                    intComp <- evalExp exp
                    case intComp of (IntVal x) -> return $ IntVal (-x)
                                    _ -> throwError "type error: cannot negate a non-integer"

evalExp (ECall funExp (argExp:argExps)) =
        -- f x1 x2 x3.. = (..(((f x1) x2) x3)..)
        case argExps of
            _:_ -> evalExp $ ECall (ECall funExp [argExp]) argExps
            []  -> do
                funComp <- evalExp funExp
                arg <- evalExp argExp
                case funComp of
                    (FunVal param body scope) ->
                        local (addParam param arg . const scope) (evalFunBody body)
                    _ -> throwError "type error: tried to call a non-function"
              where addParam (PIdent ident) arg = M.insert (NIdent ident) (return arg)
                    addParam _ _                = id

evalExp (EName name) = do
        env <- ask
        case M.lookup name env of
            Nothing -> throwError $ "unbound variable: " ++ showName name
            Just val -> val

evalExp (EConst const) =
        return $ case const of (CStr s)       -> StringVal s
                               (CInt i)       -> IntVal i
                               (CBool BTrue)  -> BoolVal True
                               (CBool BFalse) -> BoolVal False
                               CList          -> ListVal []

evalExp (EList listExp) = do
        list <- sequence $ map evalExp listExp
        return $ ListVal list

evalFunBody :: FunBody -> Computation Value
evalFunBody (ExpBody exp) = evalExp exp
evalFunBody (CompBody comp) = comp

evalLetBinding :: LBind -> Computation (Env -> Env)
evalLetBinding (LBind name params exp) =
        -- (let f x1 x2.. = e) == (let f = \x1 x2.. -> e)
        case params of
            []   -> return $ M.insert name (evalExp exp)
            p:ps -> let funComp = evalExp (ELam p ps exp) in
                    local (M.insert name funComp) $ do -- some magic to enable recursion
                        env <- ask
                        return $ M.insert name (local (const env) funComp)

showName :: Name -> String
showName (NIdent (Ident s)) = s
showName (NOp (IOCust (SIdent s))) = "(" ++ s ++ ")"
showName n = show n
