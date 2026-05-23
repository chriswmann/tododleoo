module Main (main) where

import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Domain (Store, completeTodo, createTodo, deleteTodo, emptyStore, getTodo, insertTodo, parseTodoTitle)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

data StoreCommand
  = Create String
  | Complete Int
  | Delete Int
  | View Int
  | List
  deriving (Show)

data MetaCommand
  = Help
  | Quit
  deriving (Show)

data Command = Store StoreCommand | Meta MetaCommand
  deriving (Show)

parseCommand :: String -> Maybe Command
parseCommand input = case words input of
  ("add" : rest) -> Just (Store (Create (unwords rest)))
  ["done", idStr] -> Store . Complete <$> readMaybe idStr
  ["remove", idStr] -> Store . Delete <$> readMaybe idStr
  ["view", idStr] -> Store . View <$> readMaybe idStr
  ["list"] -> Just (Store List)
  ["help"] -> Just (Meta Help)
  ["exit"] -> Just (Meta Quit)
  ["quit"] -> Just (Meta Quit)
  _ -> Nothing

execute :: StoreCommand -> Store -> IO Store
execute command store = case command of
  Create title -> do
    now <- getCurrentTime
    case parseTodoTitle $ T.pack title of
      Right todoTitle -> do
        let todo = createTodo now todoTitle
        let (_, updatedStore) = insertTodo todo store
        pure updatedStore
      Left err -> do
        print err
        pure store
  Complete lotId -> do
    now <- getCurrentTime
    let updatedStore = completeTodo now lotId store
    print (getTodo lotId updatedStore)
    pure updatedStore
  Delete lotId -> do
    let updatedStore = deleteTodo lotId store
    let msg = "Lot " ++ show lotId ++ " deleted."
    putStrLn msg
    pure updatedStore
  View lotId -> do
    print (getTodo lotId store)
    pure store
  List -> do
    print store
    pure store

printMenu :: IO ()
printMenu = putStrLn "Commands: add, done, remove, view, list, help, quit"

loop :: Store -> IO ()
loop store = do
  printMenu
  input <- getLine
  case parseCommand input of
    Nothing -> do putStrLn "Unknown command"; loop store
    Just (Meta Help) -> do printMenu; loop store
    Just (Meta Quit) -> putStrLn "Bye!"
    Just (Store command) -> do
      updatedStore <- execute command store
      loop updatedStore

main :: IO ()
main = do
  let store = emptyStore
  loop store
