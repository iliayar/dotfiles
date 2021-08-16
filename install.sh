#!/usr/bin/env bash

sudo ln -s "${PWD}" /etc/nixos
sudo nixos-rebuild switch --flake .
