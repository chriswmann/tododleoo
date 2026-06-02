{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (find)
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

data CommandSpec = CommandSpec
  { keyword :: String,
    parse :: [String] -> Maybe Command
  }

addSpec :: CommandSpec
addSpec =
  CommandSpec
    { keyword = "add",
      parse = Just . Store . Create . unwords
    }

doneSpec :: CommandSpec
doneSpec =
  CommandSpec
    { keyword = "done",
      parse = \case
        [idStr] -> Store . Complete <$> readMaybe idStr
        _ -> Nothing
    }

removeSpec :: CommandSpec
removeSpec =
  CommandSpec
    { keyword = "remove",
      parse = \case
        [idStr] -> Store . Delete <$> readMaybe idStr
        _ -> Nothing
    }

viewSpec :: CommandSpec
viewSpec =
  CommandSpec
    { keyword = "view",
      parse = \case
        [idStr] -> Store . View <$> readMaybe idStr
        _ -> Nothing
    }

listSpec :: CommandSpec
listSpec =
  CommandSpec
    { keyword = "list",
      parse = \_ -> Just (Store List)
    }

helpSpec :: CommandSpec
helpSpec =
  CommandSpec
    { keyword = "help",
      parse = \_ -> Just (Meta Help)
    }

exitSpec :: CommandSpec
exitSpec =
  CommandSpec
    { keyword = "exit",
      parse = \_ -> Just (Meta Quit)
    }

quitSpec :: CommandSpec
quitSpec =
  CommandSpec
    { keyword = "quit",
      parse = \_ -> Just (Meta Quit)
    }

commands :: [CommandSpec]
commands =
  [ addSpec,
    doneSpec,
    removeSpec,
    viewSpec,
    listSpec,
    helpSpec,
    exitSpec,
    quitSpec
  ]

parseCommand :: String -> Maybe Command
parseCommand input = case words input of
  [] -> Nothing
  (verb : args) ->
    find (\spec -> keyword spec == verb) commands
      >>= \s -> parse s args

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
    let msg = "Todo " ++ show todoId ++ " deleted."
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
