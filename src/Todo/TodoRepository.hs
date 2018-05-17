module Todo.TodoRepository
  ( Todos
  , getTodos
  , empty
  , add
  , delete
  ) where

import qualified Data.List as L
import           Todo.Todo

newtype Todos = Todos
  { getTodos :: [Todo]
  }

empty :: Todos
empty = Todos []

add :: Todo -> Todos -> Todos
add todo = Todos . (todo :) . getTodos

delete :: String -> Todos -> Todos
delete id todos = Todos [x | x <- getTodos todos, getId x /= id]
