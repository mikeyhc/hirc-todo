{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses
           , FlexibleInstances #-}

module Main where

import           Control.Arrow (first, second)
import           Control.Monad (when, void)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.List (isPrefixOf, find)
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as DB
import           HarkerIRC.Client
import           HarkerIRC.Types

newtype ToDoMonadT m a = ToDoMonad (StateT (Bool, [String])
                                   (ReaderT DB.Connection
                                   (HarkerClientT m)) a)
    deriving (Monad, MonadIO, Functor)
type ToDoMonad a = ToDoMonadT IO a

instance MonadReader DB.Connection (ToDoMonadT IO) where
    ask   = ToDoMonad (lift ask)
    local = local

instance MonadState (Bool, [String]) (ToDoMonadT IO) where
    state = ToDoMonad . state

instance MonadTrans ToDoMonadT where
    lift = ToDoMonad . lift . lift . lift

instance HarkerClientMonad (ToDoMonadT IO) where
    clientLift = ToDoMonad . lift . lift

runToDoMonad :: DB.Connection -> ToDoMonad () -> IO ()
runToDoMonad conn (ToDoMonad r) = runHarkerClient (runReaderT 
                                                  (evalStateT r (False, [])) 
                                                  conn)

main :: IO ()
main = do
    conn <- DB.connectSqlite3 "todo.sqlite3"
    createTable conn
    runPlugin "todo" "0.1.0.0" todo (runToDoMonad conn)

createTable :: DB.Connection -> IO ()
createTable conn = do
    stmt <- DB.prepare conn "CREATE TABLE IF NOT EXISTS todo(\
                            \ user TEXT NOT NULL, \
                            \ job TEXT NOT NULL)"
    void $ DB.execute stmt []
    DB.commit conn

todo :: ToDoMonad ()
todo = do
    msg <- getMsg
    cyd <- gets fst
    if      msg == "!help"                then help
    else if msg == "!todo"                then listToDo
    else if msg == "!youdo"               then ifauth toggleYouDo
    else if "!youdo " `isPrefixOf` msg    then youdo (drop 7 msg)   
    else if "!todo " `isPrefixOf` msg     then addToDo (drop 6 msg)
    else when ("!nodo " `isPrefixOf` msg) (removeToDo (drop 6 msg))
    when cyd checkYouDo 

checkYouDo :: ToDoMonad ()
checkYouDo = do
    youdoList <- gets snd
    user <- getUser
    case find (== user) youdoList of
        Just u -> sendReply (user ++ ", you have pending jobs")
               >> modify (second (filter (/= user)))
        _      -> return ()

help :: ToDoMonad ()
help = sendReply "!todo:               list all jobs to be done"
    >> sendReply "!todo message:       add a job"
    >> sendReply "!youdo user message: give a user a job"
    >> sendReply "!youdo:              enable/disable youdo command"
    >> sendReply "!nodo id:            remove a job"

listToDo :: ToDoMonad ()
listToDo =  do
    u <- getUser
    l <- sqlGetToDo u
    sendReply "to do"
    sendReply "====="
    printList 0 l

toggleYouDo :: ToDoMonad ()
toggleYouDo = do
    curval <- gets fst
    modify (first not)
    sendReply ("youdo is now " ++ if curval then "disabled"
                                            else "enabled")

youdo :: String -> ToDoMonad ()
youdo str = do
    canYouDo <- gets fst
    if canYouDo then handleYouDo (second tail (break (== ' ') str))
                else sendReply "youdo disabled"
  where
    handleYouDo :: (String, String) -> ToDoMonad ()
    handleYouDo (user, job) = do
        sqlAddJob user job
        sendReply $ "job added for " ++ user
        modify (second (user:))

printList :: Int -> [String] -> ToDoMonad ()
printList _ []     = return ()
printList i (x:xs) = sendReply (show i ++ ": " ++ x)
                  >> printList (i + 1) xs

sqlGetToDo :: String -> ToDoMonad [String]
sqlGetToDo u = do
    conn <- ask
    liftIO . fmap (map (DB.fromSql . head)) $ DB.quickQuery' conn 
        "SELECT job FROM todo  WHERE user=?" [ DB.toSql u ]

addToDo :: String -> ToDoMonad ()
addToDo t = do
    u <- getUser
    sqlAddJob u t
    sendReply "job added"

sqlAddJob :: String -> String -> ToDoMonad ()
sqlAddJob u j = do
    conn <- ask
    liftIO . void $ DB.run conn "INSERT INTO todo VALUES(?,?)"
                    [ DB.toSql u, DB.toSql j ]
    liftIO $ DB.commit conn

removeToDo :: String -> ToDoMonad ()
removeToDo t = 
    if not $ isInteger t then sendReply "not a valid id"
    else do
        let i = read t 
        u   <- getUser
        sqlRemoveToDo u i
  where
    isInteger s = case reads s :: [(Integer, String)] of
        [(_, "")] -> True
        _         -> False

sqlRemoveToDo :: String -> Int -> ToDoMonad ()
sqlRemoveToDo u i = do
    conn <- ask
    idl <- liftIO . fmap (map head) $ DB.quickQuery' conn
        "SELECT rowid FROM todo WHERE user=?" [ DB.toSql u ]
    case getElem i idl of
        Nothing -> sendReply "id out of bounds"
        Just e  -> do
            liftIO . void $ DB.run conn "DELETE FROM todo WHERE rowid=?"
                            [ e ]
            liftIO $ DB.commit conn
            sendReply "job removed"

  where
    getElem :: Int -> [a] -> Maybe a
    getElem _ []     = Nothing
    getElem 0 (x:_)  = Just x
    getElem i (_:xs) = getElem (i - 1) xs
