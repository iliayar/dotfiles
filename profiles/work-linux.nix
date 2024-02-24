{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "iliayar";
  home.homeDirectory = "/home/iliayar";

  denv = {
    langs.protobuf.enable = true;

    langs.nix.enable = true;
    langs.lua.enable = true;
    tools.grpc.enable = true;

    denv.packages = with pkgs; [ protobuf ];
  };

  custom = {
    settings = { code-stats-machine = "Work"; };

    dev = {
      cpp.enable = true;
      python.enable = true;
    };

    editors.nvim = {
      bundles = { codeStats.enable = true; };

      enable = true;
      misc = {
        enable = true;
        code = { enable = true; };
      };
      langs.enable = [ "misc" "nix" "cpp" "protobuf" ];
      code-assist = { enable = true; };
      pretty = { status-bar.enable = true; };

      langs.cpp.clangdCommand = [
        "clangd"
        "--background-index"
        "--header-insertion=never"
        "--log=info"
        "--pretty"
        "-j=16"
        "--clang-tidy"
      ];
    };

    misc = {
      enable = true;

      git = { enable = true; };

      udiskie = false;
    };

    shell = {
      misc.enable = true;
      zsh.enable = true;
    };

    de = {
      social = true;
      audio-utils = {
        enable = true;
        withWireplumber = true;
      };

      browsers = { brave.enable = true; };

      terms = { wezterm.enable = true; };

      dunst.enable = true;
      wayland = {
        hyprland = {
          enable = true;
          kbOptions = "grp:win_space_toggle,caps:escape_shifted_capslock";
          lock.enable = true;
          startupExtra = [
            "nm-applet"
            "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
          ];
        };
        waybar = {
          enable = true;
          modules = {
            left = ms:
              with ms; [
                hyprland-submap
                hyprland-workspaces
                wlr-taskbar
                hyprland-window
              ];

            right = ms:
              with ms; [
                pulseaudio
                idle-inhibitor
                (disk {
                  name = "ssd";
                  path = "/";
                })
                cpu
                memory
                battery
                hyprland-language
                clock
                tray
              ];
          };
        };
      };
      spotify.enable = true;
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
