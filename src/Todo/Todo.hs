{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.Todo
  ( Todo
  , CreateTodoJSON (..)
  , todo
  , getId
  , getText
  , isCompleted
  ) where

import           Control.Applicative (empty)
import           Data.Aeson

data Todo = Todo
  { identifier :: String
  , text       :: String
  , completed  :: Bool
  } deriving (Show, Eq)

data CreateTodoJSON = CreateTodoJSON String Bool

instance FromJSON CreateTodoJSON where
  parseJSON (Object o) = CreateTodoJSON <$> o .: "text" <*> o .: "completed"
  parseJSON _          = empty

getId :: Todo -> String
getId = identifier

getText :: Todo -> String
getText = text

isCompleted :: Todo -> Bool
isCompleted = completed

todo :: String -> String -> Bool -> Todo
todo identifier text completed = Todo {identifier = identifier, text = text, completed = completed}

instance ToJSON Todo where
  toJSON Todo {..} = object ["identifier" .= identifier, "text" .= text, "completed" .= completed]

instance FromJSON Todo where
  parseJSON (Object o) = Todo <$> o .: "identifier" <*> o .: "text" <*> o .: "completed"
  parseJSON _ = empty
