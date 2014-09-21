runhaskell:
	runhaskell -Isrc/inc -package-db=.cabal-sandbox/x86_64-linux-ghc-7.8.2-packages.conf.d/ src/BitD.hs
install-prof:
	cabal-dev --extra-config-file=cabal-dev.config install --force-reinstalls
build-prof:
	cabal-dev build --ghc-options="-osuf p_o -prof -auto-all -caf-all"
	echo "run program with ./dist/build/bfc/ +RTS -h"
