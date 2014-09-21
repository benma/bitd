{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module BitD.Util.AMQP
       ( open
       , publishMsg
       , MsgBody(..)
       , declareQueuePersistent
       , bindQueue
       , Queue
       --, consumeMsgs
       , consumeMsgs'
       )
       where

#include <Imports.hs>

import qualified Network.AMQP as Q
import Control.Monad.Trans.Resource (ResourceT, allocate)

bitdExchange :: T.Text
bitdExchange = "bitd"

newtype Queue = Queue T.Text

open :: ResourceT IO Q.Channel
open = do
  (_, conn) <- allocate (Q.openConnection "127.0.0.1" "/" "guest" "guest") Q.closeConnection
  chan <- lift $ Q.openChannel conn
  lift $ Q.declareExchange chan Q.newExchange { Q.exchangeName = bitdExchange
                                              , Q.exchangeType = "direct"
                                              , Q.exchangeDurable = True
                                              , Q.exchangeAutoDelete = False
                                              }
  return chan


class MsgBody a where
  serialize :: SerP.Putter a
  deserialize :: SerG.Get a



publishMsg :: (MsgBody msg) => Q.Channel -> T.Text -> msg -> IO ()
publishMsg chan routingKey msg = Q.publishMsg chan bitdExchange routingKey $
                                 Q.newMsg { Q.msgBody = BSL.fromStrict $ serialize' msg
                                          , Q.msgDeliveryMode = Just Q.Persistent
                                          }

declareQueuePersistent :: Q.Channel -> T.Text -> IO (Queue, Int, Int)
declareQueuePersistent chan name = do (_, messageCount, consumerCount) <- Q.declareQueue chan $
                                                                          Q.newQueue { Q.queueName = name
                                                                                     , Q.queuePassive = False
                                                                                     , Q.queueDurable = True
                                                                                     , Q.queueExclusive = False
                                                                                     , Q.queueAutoDelete = False
                                                                                     }
                                      return (Queue name, messageCount, consumerCount)

bindQueue :: Q.Channel -> Queue -> T.Text -> IO ()
bindQueue chan (Queue name) routingKey = Q.bindQueue chan name bitdExchange routingKey

deserialize' :: MsgBody a => BS.ByteString -> a
deserialize' = either error id . SerG.runGet deserialize

serialize' :: MsgBody a => a -> BS.ByteString
serialize' k = SerP.runPut (serialize k)

--consumeMsgs :: Q.Channel -> T.Text -> IO

consumeMsgs' :: (MsgBody msg) => Q.Channel -> Queue -> ((IO (), msg) -> out) -> PC.Output out -> IO ()
consumeMsgs' chan (Queue name) f output = M.void $ Q.consumeMsgs chan name Q.Ack (M.void . STM.atomically . PC.send output . f . extract)
  where extract (msg,env) = (Q.ackEnv env, deserialize' $ BSL.toStrict $ Q.msgBody msg)

-- consumeMsgs :: (MsgBody msg) => Q.Channel -> Queue -> P.Producer (IO (), msg) IO ()
-- consumeMsgs chan queue = do
--   --(output, input, seal) <- lift $ PC.spawn' PC.Unbounded
--   (output, input) <- lift $ PC.spawn PC.Unbounded
--   lift $ consumeMsgs' chan queue id output
--   PC.fromInput input
