{-# LANGUAGE GADTs #-}

module Main (main) where

import           Control.Monad.Free  (Free (..), liftF)
import           Control.Monad.State (State, gets, modify, runState)
import qualified Data.Map            as Map

-- --
-- 1. Defining the Language (The "Algebra")
-- First, we define the set of instructions for our DSL.
-- This is a simple Generalized Algebraic Data Type (GADT).
-- --
data KVStoreF next where
  Put    :: String -> String -> (() -> next)      -> KVStoreF next
  Get    :: String -> (Maybe String -> next) -> KVStoreF next
  Delete :: String -> (() -> next)      -> KVStoreF next

-- To make it a Functor, which is required by Free
instance Functor KVStoreF where
  fmap f (Put key val next) = Put key val (f . next)
  fmap f (Get key next)     = Get key (f . next)
  fmap f (Delete key next)  = Delete key (f . next)


-- --
-- 2. Building the Free Monad
-- Now, we use the Free monad (typically from the free library) to turn our KVStoreF into a proper monad.
-- --
type KVStore = Free KVStoreF

-- KVStore is now a monad! We can use do notation to write programs with it.


-- --
-- 3. Writing a Program
-- Let's write a program using our new DSL.
-- This program doesn't do anything yet; it just builds a data structure representing the sequence of operations.
-- --

-- "Smart constructors" to make the DSL nicer to use
put :: String -> String -> KVStore ()
put key val = liftF $ Put key val id

get :: String -> KVStore (Maybe String)
get key = liftF $ Get key id

delete :: String -> KVStore ()
delete key = liftF $ Delete key id

-- Our program
myProgram :: KVStore (Maybe String)
myProgram = do
  put "name" "Alice"
  put "email" "alice@example.com"
  _ <- get "name"
  delete "email"
  get "email"

-- myProgram is now a value — a data structure that represents the sequence of put, get, and delete operations.

-- --
-- 4. Interpreting the Program
-- This is where the magic happens. We can now run our myProgram in different ways by writing interpreters.
-- --

-- The interpreter function
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


-- --
-- 4.1 A "Pretty Printing" Interpreter
-- Here, we see the unique benefit of Free Monads.
-- We can write an interpreter that doesn't execute the program but instead just describes it.
-- This is possible because myProgram is just data.
-- --

-- This interpreter just prints the steps
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

  putStrLn "--- Running logging interpreter ---"
  loggedResult <- runAsLog myProgram
  putStrLn $ "Final result from logger: " ++ show loggedResult
