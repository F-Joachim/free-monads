{-# LANGUAGE GADTs #-}

module Main (main) where

import           Control.Monad.Free  (Free (..), liftF)
import           Control.Monad.State (State, gets, modify, runState)
import qualified Data.Map            as Map

-- 1. The Language
data KVStoreF next where
  Put    :: String -> String -> (() -> next)      -> KVStoreF next
  Get    :: String -> (Maybe String -> next) -> KVStoreF next
  Delete :: String -> (() -> next)      -> KVStoreF next

instance Functor KVStoreF where
  fmap f (Put key val next) = Put key val (f . next)
  fmap f (Get key next)     = Get key (f . next)
  fmap f (Delete key next)  = Delete key (f . next)

-- 2. The Free Monad
type KVStore = Free KVStoreF

-- 3. The Program
put :: String -> String -> KVStore ()
put key val = liftF $ Put key val id

get :: String -> KVStore (Maybe String)
get key = liftF $ Get key id

delete :: String -> KVStore ()
delete key = liftF $ Delete key id

myProgram :: KVStore (Maybe String)
myProgram = do
  put "name" "Alice"
  put "email" "alice@example.com"
  _ <- get "name"
  delete "email"
  get "email"

-- 4. Interpreters
runInMemory :: KVStore a -> Map.Map String String -> (a, Map.Map String String)
runInMemory kvs initialMap = runState (interpret kvs) initialMap
  where
    interpret :: KVStore a -> State (Map.Map String String) a
    interpret (Pure a) = return a
    interpret (Free (Put key val next)) = do
      modify (Map.insert key val)
      interpret (next ())
    interpret (Free (Get key next)) = do
      mval <- gets (Map.lookup key)
      interpret (next mval)
    interpret (Free (Delete key next)) = do
      modify (Map.delete key)
      interpret (next ())

-- 4.1 Alternative Interpreter (This interpreter just prints the steps)
runAsLog :: KVStore a -> IO a
runAsLog (Pure a) = return a
runAsLog (Free (Put key val next)) = do
  putStrLn $ "Putting value '" ++ val ++ "' into key '" ++ key ++ "'"
  runAsLog (next ())
runAsLog (Free (Get key next)) = do
  putStrLn $ "Getting value from key '" ++ key ++ "'"
  -- This interpreter can't actually GET a value, so we'll pretend
  runAsLog (next (Just "some-dummy-value"))
runAsLog (Free (Delete key next)) = do
  putStrLn $ "Deleting key '" ++ key ++ "'"
  runAsLog (next ())

-- Main execution
main :: IO ()
main = do
  putStrLn "--- Running in-memory interpreter ---"
  let (result, finalState) = runInMemory myProgram Map.empty
  putStrLn $ "Final result of get 'email': " ++ show result
  putStrLn $ "Final state of the map: " ++ show finalState
