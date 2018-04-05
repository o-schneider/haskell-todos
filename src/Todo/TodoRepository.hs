module Todo.TodoRepository
  ( Todos
  , getTodos
  , empty
  , add
  ) where

import           Todo.Todo

newtype Todos = Todos
  { getTodos :: [Todo]
  }

empty :: Todos
empty = Todos []

add :: Todo -> Todos -> Todos
add todo = Todos . (todo :) . getTodos
