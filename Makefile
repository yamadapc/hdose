OS := $(shell uname)
READLINE_PATH := $(shell brew --prefix readline)
SRC = src/Main.hs src/Hdose/Logging.hs src/Hdose/Options.hs

build: dependencies configure $(SRC)
	cabal build

dependencies: hdose.cabal
	cabal sandbox init
ifeq ($(OS),Darwin)
	cabal install readline --extra-include-dirs=$(READLINE_PATH)/include \
		--extra-lib-dirs=$(READLINE_PATH)/lib \
		--configure-option=--with-readline-includes=$(READLINE_PATH)/include \
		--configure-option=--with-readline-libraries=$(READLINE_PATH)/lib
endif
	cabal install --only-dep -j4

configure: hdose.cabal
	cabal configure

node-example: build
	cabal run -- mocha node_test/test.js
