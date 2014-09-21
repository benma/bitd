#bitd

bitd is an efficient indexer for the Bitcoin blockchain written in Haskell, supporting getbalance and other queries.
The data is stored in a leveldb database.

Also contained is code to connect to a bitcoin node and stream new transactions. 

* [blockchain data format](src/BitD/Protocol/Blockchain.hs)
* [bitcoin network messages](src/BitD/Protocol/Messages.hs)
* [network connection to a bitcoin node, streaming of new transactions](src/BitD/ConnectionDaemon.hs)
* [synchronizing of index database with the blockchain](src/BitD/Sync.hs)
* [JSON web service](src/BitD/WebService.hs)

# Notes

* The project is a playing field and undocumented/uncommented.
* The mempool implementation is a rough draft and not fully functional.
* The leveldb value binary format is fixed at the moment, but should use the safecopy package
  and a format like MessagePack to allow for easy handling of format changes and better space efficiency.