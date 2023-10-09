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
        nvim-comment

        telescope-nvim
        telescope-fzf-native-nvim
        telescope-file-browser-nvim
        telescope-ui-select-nvim
        plenary-nvim

        trouble-nvim

        neogit
      ];

      config = {
        programs.neovim = { extraPackages = with pkgs; [ ripgrep ]; };
      };
    };

    statusBar = {
      autoEnable = cfg.pretty.status-bar.enable;
      plugins = with pkgs.vimPlugins; [ lualine-nvim lualine-lsp-progress ];
    };

    prettyGruvbox = {
      autoEnable = cfg.pretty.theme == "gruvbox";
      plugins = with pkgs.vimPlugins; [ gruvbox-nvim ];
    };

    codeMisc = {
      autoEnable = cfg.misc.code.enable;
      plugins = with pkgs.vimPlugins; [
        nvim-treesitter.withAllGrammars
        nvim-treesitter-context
        formatter-nvim
        nvim-cmp

        nvim-snippy
        cmp-snippy
      ];
      config = {
        programs.neovim = { extraPackages = with pkgs; [ tree-sitter ]; };
      };
    };

    linux = { autoEnable = pkgs.stdenv.isLinux; };

    codeStats = {
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
      autoEnable = cfg.code-assist.enable;
      plugins = with pkgs.vimPlugins; [
        nvim-lspconfig
        cmp-nvim-lsp
      ];
    };

    langMisc = { autoEnable = builtins.elem "misc" cfg.langs.enable; };
    langNix = { autoEnable = builtins.elem "nix" cfg.langs.enable; };
    langPython = { autoEnable = builtins.elem "python" cfg.langs.enable; };
    langRust = { autoEnable = builtins.elem "rust" cfg.langs.enable; };
    langGo = { autoEnable = builtins.elem "go" cfg.langs.enable; };
    langLua = { autoEnable = builtins.elem "lua" cfg.langs.enable; };

  };
in {
  options = {
    custom.editors.nvim = {

      bundles = foldl (acc: name:
        acc // {
          "${name}".enable = mkOption { default = false; };
        }) { } (attrNames bundles);

      enable = mkOption { default = false; };

      misc = {
        enable = mkOption { default = false; };

        code = { enable = mkOption { default = false; }; };
      };

      langs = {
        enable = mkOption {
          default = [ "misc" ];
          type = types.listOf
            (types.enum [ "misc" "nix" "python" "rust" "go" "lua" ]);
        };
      };

      code-assist = { enable = mkOption { default = false; }; };

      pretty = {
        theme = mkOption {
          default = "gruvbox";
          type = types.enum [ "gruvbox" "monokai" ];
        };

        status-bar.enable = mkOption { default = false; };
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
