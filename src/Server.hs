{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Server
  ( main )
  where

import           Control.Monad.Logger
import           Control.Monad.Trans
import qualified Data.ByteString         as B
import           Data.Int                (Int64)
import           Data.IORef
import qualified Data.Map                as M
import           Data.Maybe              (listToMaybe)
import           Data.Monoid
import qualified Data.Text               as T
import           Data.Text.Encoding      (decodeUtf8)

import           Database.Persist        hiding (get)
import           Database.Persist.Sql    (fromSqlKey)
import           Database.Persist.Sqlite hiding (get)
import           Web.Spock
import           Web.Spock.Config

import           Schema

type MySession = IORef (M.Map T.Text T.Text)
data MyAppState = DummyAppState (IORef Int)
data AppState = AppState (IORef (M.Map T.Text Int))

main :: IO ()
main = do
  ref <- newIORef M.empty
  sessionRef <- newIORef M.empty
  pool <- runStdoutLoggingT $ createSqlitePool "spock_example.db" 5
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  spockConfig <- defaultSpockCfg sessionRef (PCPool pool) (AppState ref)
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
  get "hello" $ html helloHTML
  post "hello" $ do
    nameEntry <- parseUsername <$> body
    sessId <- getSessionId 
    currentSessionRef <- readSession
    liftIO $ modifyIORef' currentSessionRef $ M.insert sessId nameEntry
    redirect "home"
  get "home" $ do
    sessId <- getSessionId 
    currentSessionRef <- readSession
    currentSession <- liftIO $ readIORef currentSessionRef
    case M.lookup sessId currentSession of
      Nothing -> redirect "hello"
      Just name -> html $ homeHTML name
  post "logout" $ do
    sessId <- getSessionId 
    currentSessionRef <- readSession
    liftIO $ modifyIORef' currentSessionRef $ M.delete sessId
    redirect "hello"

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


helloHTML :: T.Text
helloHTML =
  "<html>\
    \<body>\
      \<p>Hello! Please enter your username!\
      \<form action=\"/hello\" method=\"post\">\
        \Username: <input type=\"text\" name=\"username\"><br>\
        \<input type=\"submit\"><br>\
      \</form>\
    \</body>\
  \</html>"

homeHTML :: T.Text -> T.Text
homeHTML name =
  "<html><body><p>Hello " <> name <> 
    "</p>\
    \<form action=\"logout\" method=\"post\">\
      \<input type=\"submit\" name=\"logout_button\"<br>\
    \</form>\
  \</body>\
  \</html>" 

-- Note: 61 -> '=' in ASCII
parseUsername :: B.ByteString -> T.Text
parseUsername input = decodeUtf8 $ B.drop 1 tail_
  where
    tail_ = B.dropWhile (/= 61) input
