{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.State
  ( MonadAppState
  , monadAppState
  , runAppState
  , AppState(..)
  , emptyState
  , gets
  , modify
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Reader

import           Todo.TodoRepository

newtype AppState = AppState
  { todoState :: Todos
  }

emptyState :: AppState
emptyState = AppState empty

newtype MonadAppState a = MonadAppState
  { runAppState :: ReaderT (TVar AppState) IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar AppState))

monadAppState :: MonadTrans t => MonadAppState a -> t MonadAppState a
monadAppState = lift

gets :: (AppState -> b) -> MonadAppState b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (AppState -> AppState) -> MonadAppState ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f
