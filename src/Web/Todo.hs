{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Web.Todo
  ( routes
  ) where

import           Data.Aeson           (decode, encode)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy       as T
import           Web.Scotty.Trans

import qualified Todo.Todo            as Todo
import           Todo.TodoRepository
import           Web.State

routes :: ScottyT T.Text MonadAppState ()
routes = do
  post "/todos" $ do
    jsonData >>= (\t -> monadAppState $ modify $ (\s -> AppState (add t (todoState s))))
    redirect "/todos"

  get "/todos" $ do
    c <- monadAppState $ gets $ (\s -> getTodos (todoState s))
    json c
