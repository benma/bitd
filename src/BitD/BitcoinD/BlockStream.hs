{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module BitD.BitcoinD.BlockStream ( rawBlockStream
                                 , produceBlocks
                                 , getBlockAtOffset
                                 ) where

#include <Imports.hs>

import qualified System.Directory as Dir
import qualified Filesystem.Path.CurrentOS as Path
import Filesystem.Path.CurrentOS ((</>))

import Data.List (sort, isPrefixOf)
import BitD.Networks (Network(..))
import BitD.Types (ByteOffset)
import qualified BitD.Protocol.Blockchain as BC

blocksDirectory :: Network -> IO IO.FilePath
blocksDirectory network = do
  home <- Path.decodeString <$> Dir.getHomeDirectory
  return $ case network of
    BTCMain -> Path.encodeString $ home </> Path.decode ".bitcoin" </> Path.decode "blocks"
    BTCTest -> Path.encodeString $ home </> Path.decode ".bitcoin" </> Path.decode "testnet3" </> Path.decode "blocks"

blkFiles :: Network -> IO [IO.FilePath]
blkFiles network = do
  dir <- blocksDirectory network
  files <- sort . filter (isPrefixOf "blk") <$> (Dir.getDirectoryContents dir)
  return $ map (\p -> Path.encodeString $ Path.decodeString dir </> Path.decodeString p) files

-- | Produces one contunious byte stream as a concatenation of all the block files, as stored by bitcoind.
rawBlockStream :: (MonadIO m) => Network -> ByteOffset -> P.Producer BS.ByteString m ()
rawBlockStream network offset = do
  files <- liftIO $ blkFiles network
  M.void $ F.foldlM go (0 :: ByteOffset) files
  
  where
    go !offset' file = do
      -- liftIO $ print ("opening file " ++ file)
      hIn <- liftIO $ IO.openFile file IO.ReadMode
      fileSize <- liftIO $ IO.hFileSize hIn
      let nextOffset = offset' + fromIntegral fileSize
      M.when (nextOffset > offset) $ do
        PBS.fromHandle hIn P.>-> PBS.drop (offset - offset')
      liftIO $ IO.hClose hIn
      return nextOffset

-- | Produce a stream of blocks from a raw byte stream.
-- Produces (ByteOffsetBefore, ByteOffsetAfter, Block).
produceBlocks :: (Monad m) => Network -> ByteOffset -> (ByteOffset -> P.Producer BS.ByteString m ()) -> P.Producer (ByteOffset, ByteOffset, BC.Block) m ()
produceBlocks network offset0 rawStream = PL.evalStateP (rawStream offset0) (go offset0)
  where
    go !offset = do
      skipped <- lift $ PAP.parseL (AP.skipWhile (==0))
      let consumed' = case skipped of
            Just (Right (consumed'', ())) -> consumed''
            _ -> 0

      end <- lift PBS.isEndOfBytes
      M.unless end $ do
        result <- lift $ PB.decodeGetL $ BC.parseBlock network
        case result of
          Left _ -> return ()
          Right (consumed, b) -> do let offsetBefore = offset
                                        offsetAfter = offset + consumed + fromIntegral consumed'
                                    P.yield (offsetBefore, offsetAfter, b)
                                    go offsetAfter


getBlockAtOffset :: Network -> ByteOffset -> IO (Maybe BC.Block)
getBlockAtOffset network offset = do
  result <- P.runEffect $ PPL.head (produceBlocks network offset (rawBlockStream network))
  return $ view _3 <$> result

