{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Server
  ( main )
  where

import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Int                (Int64)
import           Data.IORef
import qualified Data.Map                as M
import           Data.Maybe              (listToMaybe)
import           Data.Monoid
import qualified Data.Text               as T

import           Database.Persist        hiding (get)
import           Database.Persist.Sql    (fromSqlKey)
import           Database.Persist.Sqlite hiding (get)
import           Web.Spock
import           Web.Spock.Config

import           Schema

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)
data AppState = AppState (IORef (M.Map T.Text Int))

main :: IO ()
main = do
  ref <- newIORef M.empty
  pool <- runStdoutLoggingT $ createSqlitePool "spock_example.db" 5
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  spockConfig <- defaultSpockCfg EmptySession (PCPool pool) (AppState ref)
  runSpock 8080 (spock spockConfig app)

app :: SpockM SqlBackend MySession AppState ()
app = do
  get root $ text "Hello World!"
  get ("hello" <//> var) $ \name -> do
    existingKeyMaybe <- runSQL $ fetchByName name
    visitorNumber <- case existingKeyMaybe of
      Nothing -> runSQL $ insertAndReturnKey name
      Just i -> return i
    text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))

updateMapWithName :: T.Text -> M.Map T.Text Int -> (M.Map T.Text Int, Int)
updateMapWithName name nameMap = case M.lookup name nameMap of
  Nothing -> (M.insert name (mapSize + 1) nameMap, mapSize + 1)
  Just i  -> (nameMap, i)
  where
    mapSize = M.size nameMap

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

fetchByName
  :: T.Text
  -> SqlPersistT (LoggingT IO) (Maybe Int64)
fetchByName name = (fmap (fromSqlKey . entityKey)) <$> 
  (listToMaybe <$> selectList [NameEntryName ==. name] [])

insertAndReturnKey
  :: T.Text
  -> SqlPersistT (LoggingT IO) Int64
insertAndReturnKey name = fromSqlKey <$> insert (NameEntry name)
