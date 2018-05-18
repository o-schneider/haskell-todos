module Todo.TodoRepository
  ( Todos
  , empty
  , get
  , add
  , delete
  ) where

import qualified Data.List as L
import           Todo.Todo

type Todos = [Todo]

empty :: Todos
empty = []

get :: String -> Todos -> Maybe Todo
get id todos = case xs of []    -> Nothing
                          (x:_) -> Just x
               where xs = [x | x <- todos, getId x == id]

add :: Todo -> Todos -> Todos
add t todos = t : todos

delete :: String -> Todos -> Todos
delete id todos = [x | x <- todos, getId x /= id]
