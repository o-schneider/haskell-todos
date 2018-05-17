module Todo.TodoRepository
  ( Todos
  , empty
  , add
  , delete
  ) where

import qualified Data.List as L
import           Todo.Todo

type Todos = [Todo]

empty :: Todos
empty = []

add :: Todo -> Todos -> Todos
add t todos = t : todos

delete :: String -> Todos -> Todos
delete id todos = [x | x <- todos, getId x /= id]
