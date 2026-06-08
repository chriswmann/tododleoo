module Main (main) where

import Command (Command (..), MetaCommand (..), StoreCommand (..), parseCommand)
import Control.Monad (when)
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Domain (Store, TodoId, completeTodo, createTodo, deleteTodo, displayTodoId, emptyStore, getTodo, insertTodo, parseTodoTitle)
import Persistence (defaultStorePath, loadStore, saveStore)
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)

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
  Complete todoId -> do
    now <- getCurrentTime
    let updatedStore = completeTodo now todoId store
    print (getTodo todoId updatedStore)
    pure updatedStore
  Delete todoId -> do
    let updatedStore = deleteTodo todoId store
    let msg = "Todo " ++ displayTodoId todoId ++ " deleted."
    putStrLn msg
    pure updatedStore
  View todoId -> do
    print (getTodo todoId store)
    pure store
  List -> do
    print store
    pure store

printMenu :: IO ()
printMenu = putStrLn "Commands: add, done, remove, view, list, help, quit"

loop :: FilePath -> Store -> IO ()
loop path store = do
  printMenu
  input <- getLine
  case parseCommand input of
    Left err -> do print err; loop path store
    Right (Meta Help) -> do printMenu; loop path store
    Right (Meta Quit) -> putStrLn "Bye!"
    Right (Store command) -> do
      updatedStore <- execute command store
      when (updatedStore /= store) (saveStore path updatedStore)
      loop path updatedStore

data YesOrNo = Yes | No
  deriving (Show, Eq)

parseYesOrNo :: String -> Maybe YesOrNo
parseYesOrNo input = case map toLower input of
  "y" -> Just Yes
  "n" -> Just No
  _ -> Nothing

confirmNewPath :: FilePath -> IO Bool
confirmNewPath fp = do
  putStrLn $ "The given store path, " ++ fp ++ ", does not exist. Do you want to create it? (y/n): "
  input <- getLine
  case parseYesOrNo input of
    Just Yes -> pure True
    Just No -> pure False
    Nothing -> confirmNewPath fp

loadExisting :: FilePath -> IO Store
loadExisting fp = do
  result <- loadStore fp
  case result of
    Left err -> do
      putStrLn ("Could not load store at: " ++ fp ++ ": " ++ show err)
      exitFailure
    Right store -> pure store

main :: IO ()
main = do
  path <- defaultStorePath
  exists <- doesFileExist path
  store <-
    if exists
      then loadExisting path
      else do
        create <- confirmNewPath path
        if create then pure emptyStore else exitSuccess
  loop path store
