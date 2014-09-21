{-# LANGUAGE CPP #-}

module BitD.Util.Daemon
       ( async
       )
       where

#include <Imports.hs>

import Control.Monad.Trans.Resource (ResourceT, allocate)

async :: IO () -> ResourceT IO ()
async m = M.void $ allocate (Async.async m) $ \t -> do
  r <- Async.waitCatch t
  case r of
    Left _ -> print "thread exited with exception"
    Right _ -> return ()
