#!/usr/bin/env bash
if [[ $# -eq 0 ]] ; then
  nix-shell -I ~ --command "cabal repl"
else
  nix-shell -I ~ --command "cabal $@"
fi
