{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses
           , FlexibleInstances #-}

module Main where

import           Control.Monad (when)
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.List (isPrefixOf)
import qualified Data.Map as M
import           HarkerIRC.Client
import           HarkerIRC.Types

newtype ToDoMonadT m a = ToDoMonad (StateT (M.Map User [String])
                                    (HarkerClientT m) a)
    deriving (Monad, MonadIO, Functor)
type ToDoMonad a = ToDoMonadT IO a

instance (Monad m) => MonadState (M.Map User [String]) (ToDoMonadT m) where
    get   = ToDoMonad get
    put   = ToDoMonad . put
    state = ToDoMonad . state

instance MonadTrans ToDoMonadT where
    lift = ToDoMonad . lift . lift

instance HarkerClientMonad (ToDoMonadT IO) where
    clientLift = ToDoMonad . lift

runToDoMonad :: ToDoMonad () -> IO ()
runToDoMonad (ToDoMonad s) = runHarkerClient (evalStateT s M.empty)

main = runPlugin "todo" "0.1.0.0" todo runToDoMonad

todo :: ToDoMonad ()
todo = do
    msg <- getMsg
    if      msg == "!help"                then help
    else if msg == "!todo"                then listToDo
    else if "!todo " `isPrefixOf` msg     then addToDo (drop 6 msg)
    else when ("!nodo " `isPrefixOf` msg) (removeToDo (drop 6 msg))

help :: ToDoMonad ()
help = sendReply "!todo:         list all jobs to be done"
    >> sendReply "!todo message: add a job"
    >> sendReply "!nodo id:      remove a job"

listToDo :: ToDoMonad ()
listToDo =  do
    u <- getUser
    ml <- gets $ M.lookup u
    sendReply "to do"
    sendReply "====="
    case ml of
        Just l -> printList 0 l
        _      -> return ()

printList :: Int -> [String] -> ToDoMonad ()
printList _ []     = return ()
printList i (x:xs) = sendReply (show i ++ ": " ++ x)
                  >> printList (i + 1) xs

addToDo :: String -> ToDoMonad ()
addToDo t = do
    u <- getUser
    modify $ M.insertWith (++) u [t]
    sendReply "job added"

removeToDo :: String -> ToDoMonad ()
removeToDo t = 
    if not $ isInteger t then sendReply "not a valid id"
    else do
        let i = read t 
        u   <- getUser
        len <- gets (maybe (-1) length . M.lookup u)
        if i >= len || i < 0 then sendReply "index out of bounds"
        else modify (M.adjust (dropAt i 0) u)
  where
    dropAt :: Int -> Int -> [a] -> [a]
    dropAt _ _ []     = []
    dropAt x y (z:zs) 
        | x == y    = zs
        | otherwise = z:dropAt x (y + 1) zs

    isInteger s = case reads s :: [(Integer, String)] of
        [(_, "")] -> True
        _         -> False
