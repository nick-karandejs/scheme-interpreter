module Environment (
    
)
where

-- mutable references in IO monad
import Data.IORef
import Control.Monad.Except
import Control.Monad
import Common

-- IORef specifies a mutable variable inside the IO monad
type Env = IORef [(String, IORef LispVal)]

-- Monad containing IO actions that may throw an error
-- We specify exception type to be LispError and the inner monad is IO
type IOThrowsError = ExceptT LispError IO

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
    env <- liftIO . readIORef envRef
    maybe
        (throwError $ UnboundVar "Setting an unbound variable" var))
        (liftIO . (flip writeIORef value))
        (lookup var env)
    return value
