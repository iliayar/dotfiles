{ config, lib, pkgs, secrets, ... }:

with lib;

let
  cfg = config.custom.editors.vscode;

  nvimInsert = "neovim.init && neovim.mode != 'insert'";
  whenNotInput =
    "!inputFocus || editorTextFocus && ${nvimInsert}";

  bundles = {
    misc = {
      autoEnable = cfg.misc.enable;
      extensions = with pkgs.vscode-extensions; [
        mkhl.direnv
        bodil.file-browser
      ];
      settings = {
        editor = {
          fontFamily = "FiraCode Nerd Font";
          fontSize = 15;

          minimap.enabled = false;
          autoClosingBrackets = "never";
          autoClosingQuotes = "never";
          lineNumbers = "relative";
        };
        workbench = {
          editor.showTabs = "single";
          activityBar.location = "hidden";
        };
        window.menuBarVisibility = "hidden";
        keyboard.dispatch = "keyCode";
      };
    };

    nvim = {
      autoEnable = cfg.ihaveautism;
      extensions = with pkgs.vscode-extensions; [ asvetliakov.vscode-neovim ];
      settings = {
        vscode-neovim = {
          neovimInitVimPaths.linux =
            "~/.config/nvim/init-vscode.lua";
        };
        extensions.experimental.affinity."asvetliakov.vscode-neovim" = 1;
      };
      config = {
        xdg.configFile."nvim/lua/config-vscode" = {
          source = ./nvim-config;
          recursive = true;
        };

        xdg.configFile."nvim/init-vscode.lua".text = ''
          require('config-vscode/config')
        '';

        custom.editors.nvim = { enable = true; };
      };

      keybindings = [
        {
          key = "space b f";
          command = "workbench.action.showEditorsInActiveGroup";
          when = whenNotInput;
        }

        {
          key = "space f f";
          command = "file-browser.open";
          when = whenNotInput;
        }
        {
          key = "space f g";
          command = "workbench.action.quickOpen";
          when = whenNotInput;
        }
        {
          key = "space f r";
          command = "search.action.openEditor";
          when = whenNotInput;
        }

        {
          key = "space o p";
          command = "workbench.action.toggleSidebarVisibility";
          when = whenNotInput;
        }
        {
          key = "space t t";
          command = "workbench.view.explorer";
          when = whenNotInput;
        }

        {
          key = "alt+x";
          command = "workbench.action.showCommands";
        }

        {
          key = "ctrl+=";
          command = "editor.action.formatDocument";
        }
      ];
    };

    langNix = {
      autoEnable = builtins.elem "nix" cfg.langs.enable;
      extensions = with pkgs.vscode-extensions; [ jnoortheen.nix-ide ];
    };

    langCoq = {
      autoEnable = builtins.elem "coq" cfg.langs.enable;
      extensions = with pkgs.vscode-extensions; [ maximedenes.vscoq ];
      settings = {
        vscoq = {
          proof.pointInterpretationMode = "NextCommand";
          completion.enable = true;
        };
      };
      keybindings = [
        {
          key = "space c l";
          command = "extension.coq.interpretToPoint";
          when = "editorTextFocus && editorLangId == 'coq' && ${nvimInsert} && neovim.mode != 'visual' ";
        }
      ];
    };

    pretty = {
      # TODO: Customize
      autoEnable = true;
      extensions = with pkgs.vscode-extensions; [ jdinhlife.gruvbox ];
      settings = {
        workbench.colorTheme = "Gruvbox Dark Hard";

        workbench.colorCustomizations = {
          editor.background = "#00000000";
          terminal.background = "#00000000";
        };
      };
    };

    codeStats = {
      extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "code-stats-vscode";
          publisher = "riussi";
          version = "1.0.18";
          sha256 = "sha256-9UyDK588qpeMWq5OBQwXYl+9qctXIDY3qd/CtosG4TU=";
        }
      ];
      settings = {
        codestats.apikey = secrets.code-stats-api-key.${config.custom.settings.code-stats-machine};
        codestats.username = "iliayar";
      };
    };

    remote = {
      autoEnable = cfg.remote.enable;
      extensions = with pkgs.vscode-extensions; [
        ms-vscode-remote.remote-ssh
      ];
      settings = {
        security.promptForRemoteFileProtocolHandling = false;
      };
    };
  };
in
{
  options = {
    custom.editors.vscode = {
      enable = mkOption { default = false; };

      package = mkOption { default = pkgs.vscode; };

      bundles = foldl
        (acc: name:
          acc // {
            "${name}".enable = mkOption { default = false; };
          })
        { }
        (attrNames bundles);

      misc = { enable = mkOption { default = true; }; };
      remote = { enable = mkOption { default = true; }; };

      ihaveautism = mkOption { default = true; };

      langs = {
        enable = mkOption {
          default = [ "misc" ];
          type = types.listOf (types.enum [ "misc" "nix" "coq" ]);
        };
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [

    (mkMerge (map
      (name:
        let bundle = { autoEnable = false; } // bundles.${name};
        in { custom.editors.vscode.bundles.${name}.enable = bundle.autoEnable; })
      (attrNames bundles)))

    (mkMerge (map
      (name:
        let
          bundleDefault = {
            autoEnable = false;
            extensions = [ ];
            settings = { };
            config = { };
            keybindings = [ ];
          };
          bundle = bundleDefault // bundles.${name};
          enabled = cfg.bundles.${name}.enable;
        in
        mkMerge [
          (mkIf enabled {
            programs.vscode = {
              extensions = bundle.extensions;
              userSettings = bundle.settings;
              keybindings = bundle.keybindings;
            };
          })
          (mkIf enabled bundle.config)
        ])
      (attrNames bundles)))

    {
      programs.vscode = {
        enable = true;
        package = cfg.package;
        mutableExtensionsDir = false;
      };
    }
  ]);
}
