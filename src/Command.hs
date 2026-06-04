{-# LANGUAGE LambdaCase #-}

module Command
  ( Command (..),
    CommandSpec,
    MetaCommand (..),
    StoreCommand (..),
    parseCommand,
  )
where

import DamerauLevenshtein (levenshtein)
import Data.List (find, minimumBy)
import Data.Ord (comparing)
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

data CommandError = UnknownVerb String (Maybe String) | BadArguments [String] | NoCommand
  deriving (Show)

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

parseCommand :: String -> Either CommandError Command
parseCommand input = case words input of
  [] -> Left NoCommand
  (verb : args) ->
    case find (\spec -> keyword spec == verb) commands of
      Nothing -> Left (UnknownVerb verb (suggest verb))
      Just spec -> case parse spec args of
        Nothing -> Left (BadArguments args)
        Just cmd -> Right cmd

suggest :: String -> Maybe String
suggest verb
  | bestDistance <= threshold = Just best
  | otherwise = Nothing
  where
    scored = [(kws, levenshtein verb kws) | spec <- commands, let kws = keyword spec]
    (best, bestDistance) = minimumBy (comparing snd) scored
    threshold = 2
