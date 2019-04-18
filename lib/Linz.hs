{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}
module Linz where

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List
import           Data.Maybe
import           Data.UUID                  (UUID, toString)
import           Data.Word
import           System.Directory
import           System.Process.Typed
import           System.Random              (randomIO)
import           Zfs

import           Text.Megaparsec
import           Text.Megaparsec.Byte
import           Text.Megaparsec.Byte.Lexer

data Node = Node
  { host :: String
  , name :: String
  }

testnode = Node
  { host = "localhost"
  , name = "main/test"
  }

create :: Node -> IO ()
create node = do
  s <- runZFSProcess (host node) $ mgetProperty syncgroup (name node)
  case s of
    Just s -> putStrLn $ "Can't create new syncgroup for dataset " ++ name node ++ " because it is already in the syncgroup " ++ s
    Nothing -> do
      -- TODO: Deterministically generate from current time, host id and dataset name
      id <- randomIO :: IO UUID
      putStrLn $ "Dataset in no syncgroup yet, creating new syncgroup with id " ++ show id
      runZFSProcess (host node) $ msetProperty syncgroup (Just (toString id)) (name node)

freeze :: Node -> IO ()
freeze Node { host, name } = do
  ms <- runZFSProcess host $ mgetProperty syncgroup name
  ro <- runZFSProcess host $ getProperty readonly name
  case ms of
    Nothing -> putStrLn "This dataset is not part of any syncgroup"
    Just s -> do
      snapshots <- runZFSProcess host $ listSnapshotsOf name
      let snapIds = mapMaybe (parseMaybe (parseMine s) . BS.pack . snapshotName) snapshots
          nextSnapId | null snapIds = 0
                     | otherwise = maximum snapIds + 1


      putStrLn $ "Creating snapshot with next snapshot id " ++ show nextSnapId
      let snapshotName = "linz-" ++ s ++ "-" ++ show nextSnapId
      runZFSProcess host $ snapshot name snapshotName

      return ()
  where
    parseMine :: String -> Parser Int
    parseMine id = string (BS.pack $ "linz-" ++ id ++ "-") *> decimal

--class LinzPrimitives m where
--  type LinzSnapshot m
--  type LinzRemote m
--  takeSnapshot :: m (LinzSnapshot m)
--  sendDiff :: LinzRemote m -> LinzSnapshot m -> LinzSnapshot m -> m ()
--  setReadable :: Bool -> m ()
--
--
--type ZFSTxg = Word64
--
--instance LinzPrimitives IO where
--  type LinzSnapshot IO = ZFSTxg
--  takeSnapshot = do
--    Just zfsBin <- return $ Just "/run/current-system/sw/bin/zfs"
--    x <- readProcessStdout_ (proc zfsBin ["get", "-Hp", "-osource,value", "com.infinisil:linz_latest", "main/test"])
--    let (sourceText, valueText) = BS.break (=='\t') x
--    putStrLn $ BS.unpack x
--    return 0
