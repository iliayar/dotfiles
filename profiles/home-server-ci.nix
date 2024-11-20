{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "ci";
  home.homeDirectory = "/home/ci";

  home.packages = with pkgs; [ git caddy ];

  custom.de.fonts.enable = false;

  systemd.user.services.ucid = let
    runtimeDeps = with pkgs; [
      git
      caddy
      # NOTE: Git transitivly requires ssh
      openssh

      coreutils
      bash
    ];
  in {
    Unit = { Description = "uCI daemon"; };
    Install = { WantedBy = [ "default.target" ]; };
    Service = {
      Type = "simple";
      ExecStart = ''
        ${pkgs.ucid}/bin/ucid --config-repo /home/ci/uci-config/ --prefix ""'';
      Restart = "on-failure";
      Environment = [
        "RUST_LOG=info"
        "HOME=/home/ci"
        "PATH=${pkgs.lib.makeBinPath runtimeDeps}"
      ];
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";
}
