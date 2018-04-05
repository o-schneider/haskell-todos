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
spec = do
  describe "todos management" $ do
    it "create empty todos" $ do
      (getTodos empty) `shouldBe` []

    it "add todos" $ do
        let newTodo = todo "text" True
        (getTodos . (add newTodo)) empty `shouldBe` [newTodo]
