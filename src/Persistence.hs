module Persistence
  ( LoadError (..),
    defaultStorePath,
    loadStore,
    saveStore,
  )
where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Domain (Store)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory, renameFile)
import System.FilePath (takeDirectory, (</>))

newtype LoadError = FileCorruptedError String
  deriving (Show, Eq)

defaultStorePath :: IO FilePath
defaultStorePath = do
  dir <- getXdgDirectory XdgData "todoleoo"
  pure (dir </> "todoleoos.json")

loadStore :: FilePath -> IO (Either LoadError Store)
loadStore fp = do
  result <- eitherDecodeFileStrict' fp
  case result of
    Left err -> pure (Left (FileCorruptedError err))
    Right store -> pure (Right store)

saveStore :: FilePath -> Store -> IO ()
saveStore fp s = do
  createDirectoryIfMissing True (takeDirectory fp)
  let tmp = fp ++ ".tmp"
  BL.writeFile tmp (encodePretty s)
  renameFile tmp fp
