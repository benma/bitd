#bitd

bitd is an efficient indexer for the Bitcoin blockchain written in Haskell, supporting getbalance and other queries.
The data is stored in a leveldb database.

* [blockchain parser instances](src/BitD/Protocol/Blockchain.hs)
* [synchronizing of index database with the blockchain](src/BitD/Sync.hs)
* [JSON web service](src/BitD/WebService.hs)

# Notes

* The project is a playing field and undocumented/uncommented.
* The mempool implementation is a rough draft and not fully functional.
* The leveldb value binary format is fixed at the moment, but should use the safecopy package
  and a format like MessagePack to allow for easy handling of format changes and better space efficiency.