{ config, lib, pkgs, code-stats-vim, secrets, ... }:

with lib;

let
  cfg = config.custom.editors.nvim;
in
  {
    options = {
      custom.editors.nvim = {
        enable = mkOption {
          default = false;
        };

      code-stats = mkOption {
        default = false;
      };

      lsp = mkOption {
        default = false;
      };

      search = mkOption {
        default = true;
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      xdg.configFile."nvim/colors" = {
        source = ./colors;
        recursive = true;
      };

      home.packages = [ pkgs.xsel ];

      home.sessionVariables = {
        EDITOR = "vim";
      };

      xdg.configFile."nvim/lua/config" = {
        source = ./config;
        recursive = true;
      };

      xdg.configFile."nvim/lua/config/nix.lua".text = ''
        local nixcfg = {}
      '' + (if cfg.code-stats then ''
        nixcfg.codestats = {
          enable = true,
          key = '${secrets.code-stats-api-key.${config.custom.settings.code-stats-machine}}',
        }
      '' else ''
        nixcfg.codestats = {
          enable = false,
        }
      '') + (if cfg.lsp then ''
        nixcfg.lsp = {
          enable = true,
        }
      '' else ''
        nixcfg.lsp = {
          enable = false,
        }
      '') + (if cfg.search then ''
        nixcfg.search = {
          enable = true,
        }
      '' else ''
        nixcfg.search = {
          enable = false,
        }
      '') + ''
        return nixcfg
      '';

      programs.neovim = {
        enable = true;
        vimAlias = true;
        plugins = with pkgs.vimPlugins; [
          vim-nix
          vim-airline
          vim-surround
          vim-gitgutter
          editorconfig-vim
          vim-easymotion

          nvim-tree-lua
          nvim-web-devicons

          nvim-comment
        ];

        extraLuaConfig = ''
          require('config/config')
        '';
      };
    }
    (mkIf cfg.code-stats {
      programs.neovim = {
        plugins = with pkgs.vimPlugins; [
          (pkgs.vimUtils.buildVimPluginFrom2Nix { name = "codestats-nvim"; src = code-stats-vim; })
        ];
      };
    })
    (mkIf cfg.lsp {
      programs.neovim = {
        plugins = with pkgs.vimPlugins; [
          nvim-lspconfig
          nvim-cmp
          cmp-nvim-lsp
          nvim-snippy
          cmp-snippy
        ];
      };
    })
    (mkIf cfg.search {
      programs.neovim = {
        plugins = with pkgs.vimPlugins; [
          telescope-nvim
          telescope-fzf-native-nvim
          plenary-nvim
        ];

        extraPackages = with pkgs; [ ripgrep ];
      };
    })
  ]);
}
