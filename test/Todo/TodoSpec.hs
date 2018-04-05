{-# LANGUAGE OverloadedStrings #-}

module Todo.TodoSpec
  ( main
  , spec
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import           Test.Hspec

import           Todo.Todo

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Todo" $ do
    it "creates todos" $ do
      let id = "id"
          text = "text"
          completion = True
          t = todo id text completion
      getId t `shouldBe` id
      getText t `shouldBe` text
      isCompleted t `shouldBe` completion

    describe "JSON" $ do
      it "toJson" $ do
        let json = encode $ todo "id" "text" True
        json `shouldBe` "{\"text\":\"text\",\"identifier\":\"id\",\"completed\":true}"

      it "fromJson" $ do
        let json = "{\"identifier\":\"id\",\"text\":\"text\",\"completed\":true}"
            decodedTodo = decode $ C.pack json
        decodedTodo `shouldBe` Just (todo "id" "text" True)
