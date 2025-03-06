{ pkgs, ... }:

pkgs.vimUtils.buildVimPlugin {
  pname = "nvim-exp";
  version = "0.0.1";
  src = ./.;
}
