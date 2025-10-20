{ config
, lib
, pkgs
, codestats-nvim
, secrets
, remote-nvim
, coq-lsp-nvim
, typst-preview-nvim
, system
, cangjie-nvim
, ...
}:

with lib;

let
  remote-nvim-pkg = pkgs.vimUtils.buildVimPlugin {
    pname = "remote-nvim.nvim";
    version = "2024-10-09";
    src = remote-nvim;

    dontPatchShebangs = true;
  };

  coq-lsp-nvim-pkg = pkgs.vimUtils.buildVimPlugin {
    pname = "coq-lsp.nvim";
    version = "2024-10-13";
    src = coq-lsp-nvim;
  };

  typst-preview-nvim-pkg = pkgs.vimUtils.buildVimPlugin {
    pname = "typst-preview.nvim";
    version = "v1.3.2-iliayar";
    src = typst-preview-nvim;
  };

  # Checks don't pass
  sonic-pi-nvim-pkg = pkgs.vimUtils.buildVimPlugin {
    pname = "sonicpi.nvim";
    version = "2025-06-22";
    src = pkgs.fetchFromGitHub {
      owner = "magicmonty";
      repo = "sonicpi.nvim";
      rev = "9204aec1461520ccb42deb6354a514ecf7eadad8";
      hash = "sha256-seqnwft6dQ9nQaBbaMnWHTUpUq7n+VcGKhBIRPZEw/c=";
    };
  };

  strudel-nvim-pkg = pkgs.vimUtils.buildVimPlugin {
    pname = "strudel.nvim";
    version = "2025-06-22";
    src = pkgs.fetchFromGitHub {
      owner = "gruvw";
      repo = "strudel.nvim";
      rev = "9ad3634c7c302f16db889a55c2ff13e66f56ded2";
      hash = "sha256-SSD76hTVKZmCBT5sfji/fC8MExO2sZBumHM+rPIF4vQ=";
    };
  };

  alabaster-nvim = pkgs.vimUtils.buildVimPlugin {
    pname = "alabaster.nvim";
    version = "2025-10-16";
    src = pkgs.fetchFromGitHub {
      owner = "p00f";
      repo = "alabaster.nvim";
      rev = "b14f4527bd5d5528cac33599f71ad542c2f38748";
      hash = "sha256-W9F9cWJfBglZ92W9h4uaVe7vENCf0wWDK0vr4U5LePU=";
    };
  };

  telescope-lines-nvim-pkg = pkgs.vimUtils.buildVimPlugin {
    pname = "telescope-lines.nvim";
    version = "2025-21-02";
    src = pkgs.fetchFromGitHub {
      owner = "neanias";
      repo = "telescope-lines.nvim";
      rev = "a937349b31505ba052b973de217e7536abd813dd";
      hash = "sha256-uX24DnsJ/I7mQv2o5yfUTVOxPa/RoCvG55CWylRipZU=";
    };
  };

  cfg = config.custom.editors.nvim;

  nvim-exp = import ./nvim-exp { inherit pkgs; };

  toLuaArray = l: "{ " + (foldl (acc: e: acc + ''"${e}", '') "" l) + "}";

  bundles = {
    misc = {
      autoEnable = cfg.misc.enable;
      plugins = with pkgs.vimPlugins; [
        nvim-surround
        nvim-web-devicons
        hop-nvim
        gitsigns-nvim
        git-conflict-nvim
        comment-nvim
        undotree

        plenary-nvim
        harpoon

        trouble-nvim

        neogit

        vim-just
        vim-llvm
      ];

      config = {
        programs.neovim = { extraPackages = with pkgs; [ ripgrep ]; };
      };
    };

    remote = { plugins = with pkgs.vimPlugins; [ nui-nvim remote-nvim-pkg ]; };

    statusBar = {
      autoEnable = cfg.pretty.status-bar.enable;
      plugins = with pkgs.vimPlugins; [ lualine-nvim lualine-lsp-progress ];
    };

    todoComments = {
      autoEnable = cfg.pretty.todo-comments.enable;
      plugins = with pkgs.vimPlugins; [ todo-comments-nvim ];
    };

    prettyGruvbox = {
      autoEnable = cfg.pretty.theme == "gruvbox";
      plugins = with pkgs.vimPlugins; [ gruvbox-nvim ];
    };

    prettyAlabaster = {
      autoEnable = cfg.pretty.theme == "alabaster";
      plugins = with pkgs.vimPlugins; [ alabaster-nvim ];
    };

    codeMisc = {
      autoEnable = cfg.misc.code.enable;
      plugins = with pkgs.vimPlugins; [
        (nvim-treesitter.withPlugins (ps:
          with ps;
          nvim-treesitter.allGrammars ++ cfg.misc.code.treeSitterExtraGrammars ++ [
            cangjie-nvim.packages.${system}.tree-sitter-cangjie
          ]))
        nvim-treesitter-context
        formatter-nvim
        nvim-cmp
        vim-visual-multi

        nvim-snippy
        cmp-snippy
        cmp-buffer
        nvim-lint
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
            src = codestats-nvim;
            doCheck = false;
          })
        ];
      extraParameters = {
        key = ''
          "${
            secrets.code-stats-api-key.${config.custom.settings.code-stats-machine}
          }"; '';
      };
    };

    lsp = {
      autoEnable = cfg.code-assist.enable;
      plugins = with pkgs.vimPlugins; [ nvim-lspconfig cmp-nvim-lsp ];
    };

    picker = {
        autoEnable = true;
        extraParameters = {
            t = ''"${cfg.picker}"'';
        };
        plugins = with pkgs.vimPlugins; 
            if cfg.picker == "telescope" then [
                telescope-nvim
                telescope-fzf-native-nvim
                telescope-file-browser-nvim
                telescope-ui-select-nvim
                telescope-lines-nvim-pkg
                telescope-live-grep-args-nvim
            ] else if cfg.picker == "snacks" then [
                snacks-nvim
                # Snacks explorer sucks
                yazi-nvim
            ] else [];
    };

    debugger = {
        autoEnable = cfg.misc.debugger.enable;
      plugins = with pkgs.vimPlugins; [ nvim-dap nvim-dap-virtual-text ];
    };

    langMisc = { autoEnable = builtins.elem "misc" cfg.langs.enable; };
    langNix = { autoEnable = builtins.elem "nix" cfg.langs.enable; };
    langPython = { autoEnable = builtins.elem "python" cfg.langs.enable; };
    langRust = { autoEnable = builtins.elem "rust" cfg.langs.enable; };
    langGo = { autoEnable = builtins.elem "go" cfg.langs.enable; };
    langLua = { autoEnable = builtins.elem "lua" cfg.langs.enable; };
    langOcaml = { autoEnable = builtins.elem "ocaml" cfg.langs.enable; };
    langSql = { autoEnable = builtins.elem "sql" cfg.langs.enable; };
    langLatex = { autoEnable = builtins.elem "latex" cfg.langs.enable; };
    langProtobuf = { autoEnable = builtins.elem "protobuf" cfg.langs.enable; };
    langTypescript = { autoEnable = builtins.elem "typescript" cfg.langs.enable; };
    langZig = { autoEnable = builtins.elem "zig" cfg.langs.enable; };
    langCpp = {
      autoEnable = builtins.elem "cpp" cfg.langs.enable;
      extraParameters = {
        command = toLuaArray cfg.langs.cpp.clangdCommand;
        lsp = ''"${cfg.langs.cpp.lsp}"'';
      };
    };
    langTypst = {
      autoEnable = builtins.elem "typst" cfg.langs.enable;
      plugins = with pkgs.vimPlugins; [ typst-vim typst-preview-nvim-pkg ];
      extraParameters = { } // (
        let remote = cfg.langs.typst.remote;
        in if remote != null then {
          remote = "true";
          host = ''"${remote.host}"'';
          dataPlanePort = toString remote.data-plane-port;
          controlPlanePort = toString remote.control-plane-port;
        } else {
          remote = "false";
        }
      );
    };
    langPlantuml = {
      autoEnable = builtins.elem "plantuml" cfg.langs.enable;
      plugins = with pkgs.vimPlugins; [ plantuml-syntax ];
    };
    langHaskell = { autoEnable = builtins.elem "haskell" cfg.langs.enable; };

    langLean = {
      autoEnable = builtins.elem "lean" cfg.langs.enable;
      plugins = with pkgs.vimPlugins; [ lean-nvim ];
    };
    langCoq = {
      autoEnable = builtins.elem "coq" cfg.langs.enable;
      plugins = with pkgs.vimPlugins;
        [
          Coqtail
          # NOTE: Pretty bad
          # coq-lsp-nvim-pkg 
        ];
    };
    langFSharp = { autoEnable = builtins.elem "fsharp" cfg.langs.enable; };
    langCangjie = {
      autoEnable = builtins.elem "cangjie" cfg.langs.enable;
      plugins = [ cangjie-nvim.packages.${system}.default ];
    };

    obsidian = {
      autoEnable = cfg.obsidian.enable;
      plugins = with pkgs.vimPlugins; [ obsidian-nvim ];
      extraParameters = { path = ''"${cfg.obsidian.path}"''; };
    };

    orgmode = { plugins = with pkgs.vimPlugins; [ orgmode ]; };

    agi = { plugins = with pkgs.vimPlugins; [ ChatGPT-nvim ]; };

    exp = { autoEnable = cfg.experiments.enable; plugins = [ nvim-exp ]; };

    sonicpi = { plugins = [ sonic-pi-nvim-pkg ]; };
    strudel = { plugins = [ strudel-nvim-pkg ]; };
  };
in
{
  options = {
    custom.editors.nvim = {

      extraPlugins = mkOption { default = [ ]; };
      extraConfig = mkOption { default = ""; };

      bundles = foldl
        (acc: name:
          acc // {
            "${name}".enable = mkOption { default = false; };
          })
        { }
        (attrNames bundles);

      enable = mkOption { default = false; };

      misc = {
        enable = mkOption { default = false; };
        code = {
          enable = mkOption { default = false; };
          treeSitterExtraGrammars = mkOption { default = [ ]; };
        };
        debugger = {
            enable = mkOption { default = false; };
        };
      };

      picker = mkOption {
          default = "snacks";
          type = types.enum [ "snacks" "telescope" ];
      };

      langs = {
        enable = mkOption {
          default = [ "misc" ];
          type = types.listOf (types.enum [
            "misc"
            "nix"
            "python"
            "rust"
            "go"
            "lua"
            "cpp"
            "ocaml"
            "sql"
            "latex"
            "protobuf"
            "typst"
            "plantuml"
            "haskell"
            "lean"
            "coq"
            "typescript"
            "zig"
            "fsharp"
            "cangjie"
          ]);
        };

        cpp = {
          lsp = mkOption {
            default = "clangd";
            type = types.enum [ "clangd" "ccls" ];
          };
          clangdCommand = mkOption { default = [ "clangd" ]; };
        };

        typst = {
          remote = mkOption {
            default = null;
            type = types.nullOr (types.submodule {
              options = {
                host = mkOption { type = types.str; };
                control-plane-port = mkOption { type = types.int; };
                data-plane-port = mkOption { type = types.int; };
              };
            });
          };
        };
      };

      code-assist = { enable = mkOption { default = false; }; };

      pretty = {
        theme = mkOption {
          default = "gruvbox";
          type = types.enum [ "gruvbox" "monokai" "alabaster" ];
        };

        status-bar.enable = mkOption { default = false; };

        todo-comments.enable = mkOption { default = false; };
      };

      obsidian = {
        enable = mkOption { default = false; };
        path = mkOption {
          default = "~/org/obsidian/notes";
          type = types.str;
        };
      };

      experiments = {
        enable = mkOption { default = true; };
      };

      extraNixCfg = mkOption { default = ""; };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      xdg.configFile."nvim/lua/config/nix.lua".text = ''
        ${cfg.extraNixCfg}
        return nixcfg
      '';
    }

    (mkMerge (map
      (name:
        let bundle = { autoEnable = false; } // bundles.${name};
        in { custom.editors.nvim.bundles.${name}.enable = bundle.autoEnable; })
      (attrNames bundles)))

    (mkMerge (map
      (name:
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
          extraParametersConfig = foldl
            (acc: param:
              acc + ''
                nixcfg.${name}.${param} = ${bundle.extraParameters.${param}}
              '') ""
            (attrNames bundle.extraParameters);
        in
        mkMerge [
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
          (mkIf enabled bundle.config)
        ])
      (attrNames bundles)))

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
        plugins = cfg.extraPlugins;
        extraLuaConfig = ''
          require('config/config')

          ${cfg.extraConfig}
        '';
      };
    }
  ]);
}
