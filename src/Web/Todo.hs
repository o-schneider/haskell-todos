{-# LANGUAGE OverloadedStrings #-}

module Web.Todo
  ( routes
  ) where

import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (decode, encode)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy       as T
import           Data.UUID
import           Data.UUID.V4
import           Web.Scotty.Trans     as S

import qualified Todo.Todo            as Todo
import           Todo.TodoRepository  as TodoRepository
import           Web.State

routes :: ScottyT T.Text MonadAppState ()
routes = do
  get "/todos" $
    monadAppState (gets todoState) >>= json

  post "/todos" $
    (\uuid (Todo.CreateTodoJSON t c) -> Todo.todo uuid t c) <$>
    liftIO (toString <$> nextRandom) <*>
    jsonData >>=
    (\newTodo ->
       (monadAppState . modify) ((AppState . TodoRepository.add newTodo) . todoState) >>=
       (\_ -> json newTodo))

  S.delete "/todos/:id" $
    param "id" >>=
    (\id ->
       (monadAppState . modify) ((AppState . TodoRepository.delete id) . todoState) >>=
       (\_ -> (text . T.pack) ("Todo " ++ id ++ " deleted")))
