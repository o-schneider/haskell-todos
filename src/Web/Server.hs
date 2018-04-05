{-# LANGUAGE OverloadedStrings #-}

module Web.Server
  ( run
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Reader

import           Data.Aeson             (decode)
import qualified Data.Text.Lazy         as T
import           Web.Scotty.Trans

import           Web.State
import           Web.Todo

type Port = Int

run :: Port -> IO ()
run p = do
  sync <- newTVarIO emptyState
        -- Note that 'runM' is only called once, at startup.
  let runM m = runReaderT (runAppState m) sync
        -- 'runActionToIO' is called once per action.
      runActionToIO = runM

  scottyT p runM routes
