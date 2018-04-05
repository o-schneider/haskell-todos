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
      let text = "text"
          completion = True
          t = todo text completion
      getText t `shouldBe` text
      isCompleted t `shouldBe` completion

    describe "JSON" $ do
      it "toJson" $ do
        let json = encode $ todo "text" True
        json `shouldBe` "{\"text\":\"text\",\"completed\":true}"

      it "fromJson" $ do
        let text = "text"
            completion = True
            json = "{\"text\":\"" ++ text ++ "\",\"completed\":true}"
            decodedTodo = decode $ C.pack json
        decodedTodo `shouldBe` Just (todo text completion)
