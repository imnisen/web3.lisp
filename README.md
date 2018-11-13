

# Web3.lisp

Under active development

Inspire and steal from  <https://github.com/tsikov/ethi>


## Usage

1.  Clone to somewhere `quicklisp` can find, such as `/quicklisp/local-projects`
2.  Run `(ql:quickload :web3)` in lisp repl to load it
3.  Demo: call `web3_clientVersion`

    
    ;; set global web3:*provider*, then call method
    
    (setf web3:*provider* (make-instance 'web3:HTTPprovider :uri "http://localhost:8545"))
    
    (web3:web3/client-version)
    
    
    ;; Or pass provider as key argument
    
    (web3/client-version :key provider (make-instance 'web3:HTTPprovider :uri "http://localhost:8545"))


## Installation

To run the tests locally you will need to run a local private testnet.

1.  Install `geth`
2.  `cd` to the project's directory and start it with:

    ;; First, init genesis block
    
    geth --rpc --nodiscover --maxpeers 0 --datadir "tests/client-data" init tests/CustomGenesis.json
    
    ;; Second, run geth
    
    geth --nodiscover --maxpeers 0 --rpc --datadir tests/client-data --networkid 555 --unlock "0,1" --password tests/password.txt console
    
    Explain:
    - `--rpc` enables the rpc. Duh...
    - `--nodiscover` will make sure your node is not discoverable.
    - `--maxpeers 0` will prevent the node from syncing.
    - `init t/CustomGenesis.json` will ensure you create a custom testing blockchain.
    - `--unlock "0,1"` will unlock the first two accounts
    - `--password` will use the password provided in the text file
    - `console` will open the console

1.  In the step2 opend console, run `miner.start()` to mine some blocks. After a while, execute `miner.stop()` to stop mining.
2.  Run `(asdf:test-system :web3)` inside your repl. I am assuming you cloned the project in a directory that `asdf` can see.


## Author

-   Nisen (imnisen@gmail.com)


## Copyright

This project comes with a [BSD-style license](https://opensource.org/licenses/bsd-license.php) so you can basically do with it whatever you want.

