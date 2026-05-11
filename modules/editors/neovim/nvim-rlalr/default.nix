{ pkgs, ... }:

pkgs.vimUtils.buildVimPlugin {
  pname = "nvim-rlalr";
  version = "0.0.1";
  src = ./.;
}
