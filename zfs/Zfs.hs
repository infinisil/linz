{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Zfs where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BS
import           System.Process.Typed

import           Data.Void
import           Data.Word
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer


type Parser = Parsec Void String


data ZFSProcess a = ZFSProcess
  { zfsArguments :: [String]
  , zfsParser    :: Parser a
  }

runZFSProcess :: MonadIO m => ZFSProcess a -> m a
runZFSProcess ZFSProcess { zfsArguments, zfsParser } = do
  stdout <- readProcessStdout_ (proc "/run/current-system/sw/bin/zfs" zfsArguments)
  case parse zfsParser "zfs-output" (BS.unpack stdout) of
    Left err    -> fail (errorBundlePretty err)
    Right value -> return value

type Property = String
type Dataset = String

getProperty :: Property -> Dataset -> ZFSProcess (Maybe String)
getProperty prop set = ZFSProcess
  { zfsArguments = ["get", "-Hp", "-osource,value", prop, set]
  , zfsParser = do
      source <- many (anySingleBut '\t')
      case source of
        "-" -> return Nothing
        _   -> Just <$> (char '\t' *> many (anySingleBut '\n') <* char '\n')

  }
