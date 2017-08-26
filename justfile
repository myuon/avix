build:
  stack build

docs:
  rm -r ./docs
  cp -r .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/doc/html/avix/ ./docs

