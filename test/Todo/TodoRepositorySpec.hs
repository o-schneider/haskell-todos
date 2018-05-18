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
      empty `shouldBe` []

    it "get todos by id" $ do
      let idToFind = "id1"
          todo1 = todo idToFind "text" True
          todo2 = todo "idToKeep" "text" True
          todos = add todo1 (add todo2 empty)
      get idToFind todos `shouldBe` Just todo1

    it "get todos by id" $ do
      let idToFind = "id1"
          todo1 = todo "id2" "text" True
          todos = add todo1 empty
      get idToFind todos `shouldBe` Nothing

    it "add todo" $ do
      let newTodo = todo "id" "text" True
      add newTodo empty `shouldBe` [newTodo]

    it "remove todo" $ do
      let id = "id"
          t = todo id "text" True
          todos = add t empty
      delete id todos `shouldBe` []
