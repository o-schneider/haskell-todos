{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.Todo
  ( Todo
  , todo
  , getText
  , isCompleted
  ) where

import           Control.Applicative (empty)
import           Data.Aeson

data Todo = Todo
  { text      :: String
  , completed :: Bool
  } deriving (Show, Eq)

getText :: Todo -> String
getText = text

isCompleted :: Todo -> Bool
isCompleted = completed

todo :: String -> Bool -> Todo
todo text completed = Todo {text = text, completed = completed}

instance ToJSON Todo where
  toJSON Todo {..} = object ["text" .= text, "completed" .= completed]

instance FromJSON Todo where
  parseJSON (Object o) = Todo <$> o .: "text" <*> o .: "completed"
  parseJSON _          = empty
