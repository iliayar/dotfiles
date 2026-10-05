{
  config,
  pkgs,
  lib,
  themes,
  ...
}:

with lib;

let
  cfg = config.custom.misc;
in
{
  imports = [
    ./vcs.nix
    ./ssh.nix
    ./gpg.nix
    ./password-store.nix
    ./mail.nix
    ./zellij
    ./net.nix
  ];

  options = {
    custom.misc = {
      enable = mkOption {
        default = false;
      };

      syncthing = mkOption {
        default = false;
      };

      udiskie = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        killall
        unzip
        zip
        pandoc
        poppler-utils
        htop
        bottom

        ripgrep
        fd
        bat
        procs
        sd
        dust
        tokei
        delta
        hurl
        jq
        parallel
        mprocs
        just
        util-linux
      ];

      programs.yazi = {
        enable = true;
        shellWrapperName = "y";

        plugins =
          let
            ucp-yazi = pkgs.fetchFromGitHub {
              owner = "Fun10165";
              repo = "ucp.yazi";
              rev = "fix-macos-clipboard-paste";
              sha256 = "sha256-0pZKSSC4vnz2DArdKZFx5ixp7tgt4HswRnTTz8puNo0=";
            };
          in
          {
            ucp = "${ucp-yazi}";
          }
          // (
            if pkgs.stdenv.hostPlatform.isDarwin then
              {
                mactag = {
                  package = pkgs.yaziPlugins.mactag;
                  setup = true;
                  settings = {
                    keys = {
                      r = "Red";
                      o = "Orange";
                      y = "Yellow";
                      g = "Green";
                      b = "Blue";
                      p = "Purple";
                    };
                    colors = {
                      Red = "#ee7b70";
                      Orange = "#f5bd5c";
                      Yellow = "#fbe764";
                      Green = "#91fc87";
                      Blue = "#5fa3f8";
                      Purple = "#cb88f8";
                    };
                    order = 500;
                  };
                };
              }
            else
              { }
          );

        keymap = {
          mgr.prepend_keymap = [
            {
              on = "p";
              run = "plugin ucp paste notify";
              desc = "Paste";
            }
            {
              on = "y";
              run = "plugin ucp copy notify";
              desc = "Copy";
            }
          ]
          ++ (
            if pkgs.stdenv.hostPlatform.isDarwin then
              [
                {
                  on = [
                    "b"
                    "a"
                  ];
                  run = "plugin mactag add";
                  desc = "Tag selected files";
                }
                {
                  on = [
                    "b"
                    "r"
                  ];
                  run = "plugin mactag remove";
                  desc = "Untag selected files";
                }
              ]
            else
              [ ]
          );
        };

        settings = {
          plugin.prepend_fetchers =
            if pkgs.stdenv.hostPlatform.isDarwin then
              [
                {
                  url = "*";
                  run = "mactag";
                  group = "mactag";
                }
                {
                  url = "*/";
                  run = "mactag";
                  group = "mactag";
                }
              ]
            else
              [ ];
        };
      };
    }
    (mkIf cfg.syncthing {
      services.syncthing.enable = true;
    })
    (mkIf cfg.udiskie {
      services.udiskie = {
        enable = true;
        tray = "never";
      };
    })
  ]);
}
