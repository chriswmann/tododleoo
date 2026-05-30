module Main (main) where

import Control.Monad (when)
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Domain (Store, completeTodo, createTodo, deleteTodo, emptyStore, getTodo, insertTodo, parseTodoTitle)
import Persistence (defaultStorePath, loadStore, saveStore)
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)
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

loop :: FilePath -> Store -> IO ()
loop path store = do
  printMenu
  input <- getLine
  case parseCommand input of
    Nothing -> do putStrLn "Unknown command"; loop path store
    Just (Meta Help) -> do printMenu; loop path store
    Just (Meta Quit) -> putStrLn "Bye!"
    Just (Store command) -> do
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
