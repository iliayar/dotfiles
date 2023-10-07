{ config, lib, pkgs, code-stats-vim, secrets, ... }:

with lib;

let
  cfg = config.custom.editors.nvim;

  bundles = {
    misc = {
      autoEnable = cfg.misc.enable;
      plugins = with pkgs.vimPlugins; [
        nvim-surround
        nvim-web-devicons
        hop-nvim
        gitsigns-nvim
        nvim-window-picker
      ];
    };

    statusBar = {
      autoEnable = cfg.pretty.status-bar.enable;
      plugins = with pkgs.vimPlugins; [ vim-airline ];
    };

    prettyGruvbox = {
      autoEnable = cfg.pretty.theme == "gruvbox";
      plugins = with pkgs.vimPlugins; [ gruvbox-nvim ];
    };

    codeMisc = {
      autoEnable = cfg.code.misc.enable;
      plugins = with pkgs.vimPlugins; [
        nvim-comment
        nvim-treesitter.withAllGrammars
        nvim-treesitter-context
        formatter-nvim
      ];
      config = {
        programs.neovim = { extraPackages = with pkgs; [ tree-sitter ]; };
      };
    };

    linux = { autoEnable = pkgs.stdenv.isLinux; };
    codeStats = {
      autoEnable = cfg.misc.enable && cfg.misc.code-stats.enable;
      plugins = with pkgs.vimPlugins;
        [
          (pkgs.vimUtils.buildVimPlugin {
            name = "codestats-nvim";
            src = code-stats-vim;
          })
        ];
      extraParameters = {
        key =
          "${secrets.code-stats-api-key.${config.custom.settings.code-stats-machine}}";
      };
    };
    lsp = {
      autoEnable = cfg.code.lsp.enable;
      plugins = with pkgs.vimPlugins; [
        nvim-lspconfig
        nvim-cmp
        cmp-nvim-lsp
        nvim-snippy
        cmp-snippy
      ];
    };
    search = {
      autoEnable = cfg.misc.enable && cfg.misc.search.enable;

      plugins = with pkgs.vimPlugins; [
        telescope-nvim
        telescope-fzf-native-nvim
        telescope-file-browser-nvim
        telescope-ui-select-nvim
        plenary-nvim

        trouble-nvim
      ];

      config = {
        programs.neovim = { extraPackages = with pkgs; [ ripgrep ]; };
      };
    };
    tree = {
      autoEnable = cfg.misc.enable && cfg.misc.tree.enable;
      plugins = with pkgs.vimPlugins; [ nvim-tree-lua ];
    };
    neogit = {
      autoEnable = cfg.misc.enable && cfg.misc.git.enable;

      plugins = with pkgs.vimPlugins; [
        neogit
        plenary-nvim
      ];
    };
  };
in {
  options = {
    custom.editors.nvim = {

      bundles = foldl (acc: name:
        acc // {
          "${name}".enable = mkOption { default = false; };
        }) { } (attrNames bundles);

      enable = mkOption { default = false; };

      pretty = {
        theme = mkOption {
          default = "gruvbox";
          type = types.enum [ "gruvbox" "monokai" ];
        };

        status-bar.enable = mkOption { default = false; };
      };

      misc = {
        enable = mkOption { default = true; };
        tree.enable = mkOption { default = false; };
        search.enable = mkOption { default = true; };
        code-stats.enable = mkOption { default = false; };
        git.enable = mkOption { default = false; };
      };

      code = {
        misc.enable = mkOption { default = true; };
        lsp.enable = mkOption { default = false; };
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      xdg.configFile."nvim/lua/config/nix.lua".text = ''
        return nixcfg
      '';
    }

    (mkMerge (map (name:
      let bundle = { autoEnable = false; } // bundles.${name};
      in { custom.editors.nvim.bundles.${name}.enable = bundle.autoEnable; })
      (attrNames bundles)))

    (mkMerge (map (name:
      let
        bundleDefault = {
          autoEnable = false;
          plugins = [ ];
          config = { };
          extraParameters = { };
        };
        bundle = bundleDefault // bundles.${name};
        enabled = cfg.bundles.${name}.enable;
        val = if enabled then "true" else "false";
        extraParametersConfig = foldl (acc: param: ''
          nixcfg.${name}.${param} = "${bundle.extraParameters.${param}}"
        '') "" (attrNames bundle.extraParameters);
      in mkMerge [
        (mkIf enabled {
          xdg.configFile."nvim/lua/config/nix.lua".text = extraParametersConfig;
        })
        ({
          xdg.configFile."nvim/lua/config/nix.lua".text = ''
            nixcfg.${name} = {
              enable = ${val},
            }
          '';
        })
        (mkIf enabled { programs.neovim = { plugins = bundle.plugins; }; })
        (mkIf enabled (bundle.config))
      ]) (attrNames bundles)))

    {
      xdg.configFile."nvim/colors" = {
        source = ./colors;
        recursive = true;
      };

      home.packages = [ pkgs.xsel ];

      home.sessionVariables = { EDITOR = "vim"; };

      xdg.configFile."nvim/lua/config" = {
        source = ./config;
        recursive = true;
      };

      xdg.configFile."nvim/lua/config/nix.lua".text = ''
        local nixcfg = {}
      '';

      programs.neovim = {
        enable = true;
        vimAlias = true;
        extraLuaConfig = ''
          require('config/config')
        '';
      };
    }
  ]);
}
