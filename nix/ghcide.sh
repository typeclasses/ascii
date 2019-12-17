#!/usr/bin/env bash

nix-shell nix/shell.nix --pure --run 'ghcide --lsp'
