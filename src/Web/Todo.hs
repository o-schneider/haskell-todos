{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Web.Todo
  ( routes
  ) where

import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (decode, encode)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy       as T
import           Data.UUID
import           Data.UUID.V4
import           Web.Scotty.Trans

import qualified Todo.Todo            as Todo
import           Todo.TodoRepository
import           Web.State

routes :: ScottyT T.Text MonadAppState ()
routes = do
  post "/todos" $ do
    (\uuid (Todo.CreateTodoJSON t c) -> Todo.todo uuid t c) <$>
      (liftIO $ toString <$> nextRandom) <*>
      jsonData >>=
      (\newTodo ->
         ((monadAppState . modify) $ (\s -> AppState (add newTodo s)) . todoState) >>=
         (\x -> json newTodo))

  get "/todos" $ do
    c <- monadAppState $ gets $ (\s -> getTodos (todoState s))
    json c
