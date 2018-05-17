module Todo.TodoRepositorySpec
  ( main
  , spec
  ) where

import           Control.Exception   (evaluate)
import           Test.Hspec

import           Todo.Todo
import           Todo.TodoRepository

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "todos management" $ do
    it "create empty todos" $
      getTodos empty `shouldBe` []

    it "add todo" $ do
      let newTodo = todo "id" "text" True
      (getTodos . add newTodo) empty `shouldBe` [newTodo]

    it "remove todo" $ do
      let id = "id"
          t = todo id "text" True
          todos = add t empty
      (getTodos . delete id) todos `shouldBe` []
