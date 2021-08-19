#!/usr/bin/env bash

sudo ln -s "${PWD}" /etc/nixos
sudo nixos-rebuild switch

ln -s "${PWD}" ~/.config/nixpkgs
nix-shell '<home-manager>' -A install

[ -e ~/.config/nixpkgs/home.nix ] && rm ~/.config/nixpkgs/home.nix
