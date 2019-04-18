{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}



module Zfs where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BS
import           System.Process.Typed

import           Data.Char
import           Data.Functor
import           Data.Proxy
import           Data.Void
import           Data.Word
import           GHC.OverloadedLabels
import           GHC.TypeLits
import           Text.Megaparsec
import           Text.Megaparsec.Byte
import           Text.Megaparsec.Byte.Lexer


type Parser = Parsec Void BS.ByteString


data ZFSProcess a = ZFSProcess
  { zfsArguments :: [String]
  , zfsParser    :: Parser a
  , zfsNeedsSudo :: Bool
  }

zfsProc :: ZFSProcess a -> String -> ProcessConfig () () ()
zfsProc ZFSProcess { zfsArguments, zfsNeedsSudo } host = proc (head arguments) (tail arguments)
  where
    sshBinary = "/run/current-system/sw/bin/ssh"
    sudoBinary = "/run/wrappers/bin/sudo"
    zfsBinary = "/run/current-system/sw/bin/zfs"

    wrapSudo = if zfsNeedsSudo then (sudoBinary:) else id
    wrapRemote = case host of
      "localhost" -> id
      _           -> \args -> sshBinary:host:args
    arguments = wrapRemote $ wrapSudo (zfsBinary:zfsArguments)

runZFSProcess :: MonadIO m => String -> ZFSProcess a -> m a
runZFSProcess host zfsProcess@ZFSProcess { zfsArguments, zfsParser, zfsNeedsSudo } = do
  stdout <- readProcessStdout_ (zfsProc zfsProcess host)
  case parse zfsParser "zfs-output" (BS.init stdout) of
    Left err    -> fail (errorBundlePretty err)
    Right value -> return value

type Dataset = String

data Access = ReadOnly
            | Writable

data Nullable = IsNullable
              | NotNullable

data Property (w :: Access) (n :: Nullable) a where
  PropertyNativeRO :: String -> Parser a -> Property ReadOnly NotNullable a
  PropertyNativeRW :: String -> Parser a -> (a -> String) -> Property Writable NotNullable a
  PropertyUser :: String -> Parser a -> (a -> String) -> Property Writable IsNullable a

propName :: Property w n a -> String
propName (PropertyNativeRO name _)   = name
propName (PropertyNativeRW name _ _) = name
propName (PropertyUser name _ _)     = name

propParse :: Property w n a -> Parser a
propParse (PropertyNativeRO _ parse)   = parse
propParse (PropertyNativeRW _ parse _) = parse
propParse (PropertyUser _ parse _)     = parse

propPretty :: Property Writable n a -> a -> String
propPretty (PropertyNativeRW _ _ pretty) = pretty
propPretty (PropertyUser _ _ pretty)     = pretty

readonly :: Property Writable NotNullable Bool
readonly = PropertyNativeRW "readonly"
  (string "on" $> True <|> string "off" $> False)
  (\b -> if b then "on" else "off")

syncgroup :: Property Writable IsNullable String
syncgroup = PropertyUser "com.infinisil.linz:syncgroup"
  (BS.unpack <$> getInput)
  id

mgetProperty :: Property w IsNullable a -> Dataset -> ZFSProcess (Maybe a)
mgetProperty prop set = ZFSProcess
  { zfsArguments = ["get", "-Hp", "-osource,value", propName prop, set]
  , zfsParser = do
      source <- many (anySingleBut (fromIntegral $ ord '\t'))
      case source of
        [45] -> return Nothing
        _    -> Just <$> (tab *> propParse prop)
  , zfsNeedsSudo = False
  }

getProperty :: Property w NotNullable a -> Dataset -> ZFSProcess a
getProperty prop set = ZFSProcess
  { zfsArguments = ["get", "-Hp", "-ovalue", propName prop, set]
  , zfsParser = propParse prop
  , zfsNeedsSudo = False
  }

msetProperty :: Property Writable IsNullable a -> Maybe a -> Dataset -> ZFSProcess ()
msetProperty prop mvalue set = ZFSProcess
  { zfsArguments = args
  , zfsParser = return ()
  , zfsNeedsSudo = True
  }
  where args = case mvalue of
          Nothing -> ["inherit", propName prop, set]
          Just value -> ["set", propName prop ++ "=" ++ propPretty prop value, set]

setProperty :: Property Writable NotNullable a -> a -> Dataset -> ZFSProcess ()
setProperty prop value set = ZFSProcess
  { zfsArguments = ["set", propName prop ++ "=" ++ propPretty prop value, set]
  , zfsParser = return ()
  , zfsNeedsSudo = True
  }



snapshot :: Dataset -> String -> ZFSProcess ()
snapshot set name = ZFSProcess
  { zfsArguments = ["snapshot", set ++ "@" ++ name]
  , zfsParser = return ()
  , zfsNeedsSudo = True
  }

data Snapshot = Snapshot
  { snapshotDataset :: Dataset
  , snapshotName    :: String
  }

instance Show Snapshot where
  show (Snapshot dataset name) = dataset ++ "@" ++ name

listSnapshotsOf :: Dataset -> ZFSProcess [Snapshot]
listSnapshotsOf set = ZFSProcess
  { zfsArguments = ["list", "-tsnapshot", "-r", "-d1", "-oname", "-Hp", set]
  , zfsParser = parseSnapshot `sepBy` char (fromIntegral $ ord '\n')
  , zfsNeedsSudo = False
  }
  where
    parseSnapshot :: Parser Snapshot
    parseSnapshot = Snapshot <$> (BS.unpack <$> string (BS.pack set) <* "@") <*> many (chr . fromIntegral <$> anySingleBut (fromIntegral $ ord '\n'))

--data Action a = End a
--              | forall p . Execute (ZFSProcess p) (p -> Action a )

--
--runAction' :: MonadIO m => Action a -> m a
--runAction' action = do
--  bin <- return "/run/current-system/sw/bin/zfs"
--  runAction bin action
--
--runAction :: MonadIO m => String -> Action a -> m a
--runAction bin = go
--  where
--    go (End x) = return x
--    go (Execute ZFSProcess { zfsArguments, zfsParser } withResult) = do
--      stdout <- readProcessStdout_ (proc bin zfsArguments)
--      case parse zfsParser "zfs-output" stdout of
--        Left err    -> fail (errorBundlePretty err)
--        Right value -> go $ withResult value

