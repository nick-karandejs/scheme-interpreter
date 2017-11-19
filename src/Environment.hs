module Environment (
    nullEnv
  , setVar
  , getVar
  , defineVar
  , bindVars
  , runIOThrows
  , liftThrows
)
where

-- mutable references in IO monad
import Data.IORef
import Control.Monad.Except
import Control.Monad
import Common

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

-- | Check if variable is already bound
isBound :: Env -> String -> IO Bool
isBound envRef var =
    -- readIORef gets the mutable list of the IORef
    -- const is a function accepting a single argument, returning given val
    readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    -- readIORef returns the IO monad, from which we lift to IOThrowsError
    -- only then can appy `<-`
    store <- liftIO $ readIORef envRef
    maybe
        (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var store)

-- | Updates a variable and returns the new value
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe
        (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    isDefined <- liftIO $ isBound envRef var
    if isDefined
        then setVar envRef var value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
{- bindVars envRef bindings = do -}
{-     let transform (var, val) = do -}
{-             ref <- newIORef val -}
{-             return (var, ref) -}
{-     newEnv <- mapM transform bindings -}
{-     env <- readIORef envRef -}
{-     writeIORef envRef (newEnv ++ env) -}
{-     return envRef -}
-- Much more elegant version
bindVars envRef bindings =
    readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, val) = do
            ref <- newIORef val
            return (var, ref)
