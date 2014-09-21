-- {-# LANGUAGE CPP #-}

-- module BitD.ShowBalance
--        ( showBalance
--        ) where

-- #include <Imports.hs>

-- import BitD.Protocol.Types (Address(..), toHex, SatoshiValue(..))
-- import BitD.MemPool as MP
-- import BitD.Protocol.Blockchain as BC
-- import qualified Database.LevelDB as LDB
-- import BitD.Protocol.BitcoinAddress (addressToHash160)
-- import qualified BitD.DB as DB
-- import BitD.DB.Types (Balance(..))

-- showBalance :: IO ()
-- showBalance = LDB.runResourceT $ do
  
--   db <- DB.initDB
--   (memPool, restoreTxs) <- MP.initMemPool
--   F.forM_ restoreTxs $ \tx -> do
--     confirmed <- DB.txIsConfirmed db (BC._txHash tx)
--     M.when (not confirmed) $ lift $ MP.addTransaction memPool tx

--   myAddresses <- (filter (not . BS.null) . BSC.lines) <$> (lift BSC.getContents)
--   let myHashes = map (fromJust . addressToHash160 . Address) myAddresses
--       conv (SatoshiValue v) = (fromIntegral v / 10**8) :: Double
--   myBalances <- M.mapM (DB.getBalance memPool db) myHashes
--   F.forM_ (zip myAddresses myBalances) $ \(address, Balance bc buc) -> lift $ putStrLn (BSC.unpack address ++ ": " ++ show (conv bc) ++ ", " ++ show (conv buc))
  
--   let Balance total_c total_uc = mconcat myBalances
      
--   lift $ putStrLn $ "Total: " ++ show (conv total_c) ++ ", " ++ show (conv total_uc)
