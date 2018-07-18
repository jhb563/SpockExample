{-# LANGUAGE OverloadedStrings #-}

module Server
  ( main )
  where

import           Control.Monad.Trans
import           Data.IORef
import qualified Data.Map            as M
import           Data.Monoid
import qualified Data.Text           as T

import           Web.Spock
import           Web.Spock.Config

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)
data AppState = AppState (IORef (M.Map T.Text Int))

main :: IO ()
main = do
  ref <- newIORef M.empty
  spockConfig <- defaultSpockCfg EmptySession PCNoDatabase (AppState ref)
  runSpock 8080 (spock spockConfig app)

app :: SpockM () MySession AppState ()
app = do
  get root $ text "Hello World!"
  get ("hello" <//> var) $ \name -> do
    (AppState mapRef) <- getState
    visitorNumber <- liftIO $ atomicModifyIORef' mapRef $ updateMapWithName name
    text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))

updateMapWithName :: T.Text -> M.Map T.Text Int -> (M.Map T.Text Int, Int)
updateMapWithName name nameMap = case M.lookup name nameMap of
  Nothing -> (M.insert name (mapSize + 1) nameMap, mapSize + 1)
  Just i -> (nameMap, i)
  where
    mapSize = M.size nameMap
