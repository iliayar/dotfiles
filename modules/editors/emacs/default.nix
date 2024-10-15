{ config, lib, pkgs, secrets, ... }@inputs:

with lib;

let
  cfg = config.custom.editors.emacs;

  makeEnableOption = name: {
    "${name}".enable = mkOption { default = false; };
  };

  makePackagesDictId =
    foldl (acc: e: acc // { "${e}" = epkgs: [ epkgs.${e} ]; }) { };

  makeEnableOptions = foldl (acc: name: acc // (makeEnableOption name)) { };

  enablePackages = pkgs:
    mkMerge
    (map (pkg: { custom.editors.emacs.packages.${pkg}.enable = true; }) pkgs);

  allPackages = makePackagesDictId [
    "ob-mermaid"
    "highlight-indent-guides"
    "obsidian"
    "topsy"
    "cheat-sh"
    "protobuf-mode"
    "pyvenv"
    "sdlang-mode"
    "vterm"
    "ace-window"
    "all-the-icons"
    "anaconda-mode"
    "avy"
    "bison-mode"
    "ccls"
    "centaur-tabs"
    "code-stats"
    "company"
    "company-anaconda"
    "company-coq"
    "company-nixos-options"
    "company-solidity"
    "consult"
    "consult-projectile"
    "dap-mode"
    "counsel-projectile"
    "corfu"
    "cape"
    "corfu-terminal"
    "marginalia"
    "orderless"
    "counsel"
    "goto-chg"
    "evil"
    "evil-collection"
    "evil-snipe"
    "kind-icon"
    "evil-surround"
    "evil-multiedit"
    "evil-mc"
    "avy"
    "ace-window"
    "hydra"
    "emacs-everywhere"
    "minimap"
    "all-the-icons"
    "hl-todo"
    "rainbow-delimiters"
    "dashboard"
    "diff-hl"
    "direnv"
    "dockerfile-mode"
    "doom-modeline"
    "doom-themes"
    "editorconfig"
    "elfeed"
    "elfeed-goodies"
    "elfeed-org"
    "emacs-everywhere"
    "esup"
    "evil"
    "evil-collection"
    "evil-mc"
    "evil-multiedit"
    "evil-snipe"
    "evil-surround"
    "exwm"
    "flycheck"
    "format-all"
    "gcmh"
    "general"
    "go-mode"
    "goto-chg"
    "graphviz-dot-mode"
    "haskell-mode"
    "hl-todo"
    "hydra"
    "impatient-mode"
    "julia-mode"
    "kotlin-mode"
    "lsp-haskell"
    "lsp-ivy"
    "lsp-java"
    "lsp-latex"
    "lsp-mode"
    "lsp-pyright"
    "lsp-treemacs"
    "lsp-ui"
    "lua-mode"
    "magit"
    "marginalia"
    "minimap"
    "minimap"
    "nix-mode"
    "nix-sandbox"
    "nixos-options"
    "orderless"
    "org-bullets"
    "org-roam"
    "org-roam-ui"
    "org-special-block-extras"
    "org-special-block-extras"
    "org-special-block-extras"
    "ox-json"
    "ox-reveal"
    "ox-gfm"
    "php-mode"
    "projectile"
    "proof-general"
    "lean4-mode"
    "rainbow-delimiters"
    "rjsx-mode"
    "rustic"
    "smartparens"
    "solidity-flycheck"
    "solidity-mode"
    "tide"
    "treemacs"
    "treemacs-evil"
    "treemacs-projectile"
    "typescript-mode"
    "undo-tree"
    "use-package"
    "vertico"
    "web-mode"
    "websocket"
    "which-key"
    "yaml-mode"
    "yasnippet"
    "yasnippet-snippets"
    "mermaid-mode"
  ] // {
    "lsp-julia" = epkgs:
      [
        (epkgs.lsp-julia.overrideAttrs
          (old: { patches = [ ./lsp-julia.patch ]; }))
      ];
    "tree-sitter-grammars" = epkgs:
      with epkgs;
      [ treesit-grammars.with-all-grammars ];
  };

  bundles = {
    basic = {
      auto-enable = true;
      packages = [ "use-package" "gcmh" "general" "hydra" "direnv" "vterm" ];
      config = {
        home.file.".emacs.d/private.el".text = ''
          ;; Some secret info here
        '';
      };
    };

    misc-inernal = {
      auto-enable = cfg.misc.enable;
      packages = [
        "projectile"
        "magit"
        # "treemacs"
        # "treemacs-projectile"
        # "minimap"
        # "esup"
        "which-key"
      ];
    };

    emacs-everywhere = {
      packages = [ "emacs-everywhere" ];
      config = { home.packages = with pkgs; [ xdotool xorg.xwininfo ]; };
    };

    misc-code-internal = {
      auto-enable = cfg.misc.enable && cfg.misc.code.enable;
      packages = [
        "editorconfig"
        "yasnippet"
        "yasnippet-snippets"
        "format-all"
        "sdlang-mode"
        "protobuf-mode"
        "tree-sitter-grammars"
        # "highlight-indent-guides"
        # "mermaid-mode"

        # "cheat-sh"
      ];
    };

    misc-code-internal-smart-parens = {
      auto-enable = cfg.misc.enable && cfg.misc.code.enable
        && cfg.misc.code.auto-parens.enable;
      packages = [ "smartparens" ];
    };

    misc-code-internal-company = {
      auto-enable = cfg.bundles.misc-code-internal.enable
        && cfg.misc.code.completion == "company";
      packages = [ "company" ];
    };

    misc-code-internal-corfu = {
      auto-enable = cfg.bundles.misc-code-internal.enable
        && cfg.misc.code.completion == "corfu";
      packages = [ "corfu" "cape" "kind-icon" "corfu-terminal" ];
    };

    langs-nix-internal = {
      auto-enable = builtins.elem "nix" cfg.langs.enable;
      packages = [
        "nix-mode"
        # "nixos-options" # FIXME: Too slow on load
      ];
    };

    langs-nix-misc-internal = {
      auto-enable = cfg.bundles.langs-nix-internal.enable
        && cfg.misc.code.enable;
      packages = [
        # "company-nixos-options" # FIXME: nixos-options too slow on load
      ];
      config = { home.packages = with pkgs; [ nixfmt-classic nixpkgs-fmt ]; };
    };

    vertico-internal = {
      auto-enable = cfg.misc.enable && cfg.misc.completion == "vertico";
      packages =
        [ "vertico" "consult" "consult-projectile" "marginalia" "orderless" ];
    };

    ivy-internal = {
      auto-enable = cfg.misc.enable && cfg.misc.completion == "ivy";
      packages = [ "counsel" "counsel-projectile" ];
    };

    org-style-v1 = { auto-enable = cfg.org.style == "v1"; };
    org-style-v2 = { auto-enable = cfg.org.style == "v2"; };

    obsidian = { packages = [ "obsidian" ]; };

    org-roam-internal = {
      auto-enable = cfg.org.roam.enable;
      packages = [ "org-roam" ];
    };

    org-roam-ui-internal = {
      auto-enable = cfg.org.roam.ui;
      packages = [ "org-roam-ui" "websocket" ];
    };

    evil-internal = {
      auto-enable = cfg.evil.enable;
      packages = [ "evil" "undo-tree" ];
    };

    evil-extra-internal = {
      auto-enable = cfg.evil.enable && cfg.evil.extra;
      packages = [ "evil-mc" "evil-surround" ];
    };

    evil-integrations = { packages = [ "evil-collection" ]; };

    evil-treemacs-internal = {
      auto-enable = cfg.evil.enable && cfg.misc.enable;
      packages = [
        # "treemacs-evil" 
      ];
    };

    theme-internal = {
      auto-enable = cfg.pretty.theme != null;
      packages = [ "doom-themes" "all-the-icons" ];
      config = {
        home.file.".emacs.d/nixcfg.el".text = ''
          (setq nixcfg-theme '${cfg.pretty.theme})
        '';
      };
    };

    pretty-extra-internal = {
      auto-enable = cfg.pretty.extra.enable;
      packages = [
        "dashboard"
        "diff-hl"

        # FIXME: This crashes emacs
        # "doom-modeline" 

        "centaur-tabs"
      ];
    };

    lsp-internal = {
      auto-enable = cfg.code-assist.enable;
      packages = [ "lsp-mode" "flycheck" ];
    };

    lsp-ui-internal = {
      auto-enable = cfg.code-assist.enable && cfg.code-assist.pretty.enable;
      packages = [ "lsp-ui" "rainbow-delimiters" ];
    };

    lsp-misc-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable && cfg.misc.enable;
      packages = [ "lsp-treemacs" ];
    };

    lsp-ivy-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.ivy-internal.enable;
      packages = [ "lsp-ivy" ];
    };

    langs-python-internal = {
      auto-enable = builtins.elem "python" cfg.langs.enable;
      packages = [ "pyvenv" ];
    };

    langs-python-lsp-pylsp-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-python-internal.enable && cfg.langs.python.ls
        == "pylsp";
      config = {
        custom.dev.python.additionalPackages = pypkgs:
          [ pypkgs.python-lsp-server ];
      };
    };

    langs-python-lsp-pyright-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-python-internal.enable && cfg.langs.python.ls
        == "pyright";
      packages = [ "lsp-pyright" ];
    };

    langs-misc-internal = {
      auto-enable = builtins.elem "misc" cfg.langs.enable;
      packages =
        [ "dockerfile-mode" "yaml-mode" "graphviz-dot-mode" "bison-mode" ];
    };

    langs-typescript-internal = {
      auto-enable = builtins.elem "typescript" cfg.langs.enable;
      packages = [ "tide" "rjsx-mode" "typescript-mode" ];
    };

    langs-cpp-internal = {
      auto-enable = builtins.elem "cpp" cfg.langs.enable;
    };

    langs-lua-internal = {
      auto-enable = builtins.elem "lua" cfg.langs.enable;
      packages = [ "lua-mode" ];
    };

    langs-cpp-lsp-ccls-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-cpp-internal.enable && cfg.langs.cpp.ls == "ccls";
      packages = [ "ccls" ];
      config = { home.packages = [ pkgs.ccls ]; };
    };

    langs-cpp-lsp-clangd-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-cpp-internal.enable && cfg.langs.cpp.ls
        == "clangd";
      config = { home.packages = [ pkgs.clang ]; };
    };

    langs-haskell-internal = {
      auto-enable = builtins.elem "haskell" cfg.langs.enable;
      packages = [ "haskell-mode" ];
    };

    langs-haskell-lsp-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-haskell-internal.enable;
      packages = [ "lsp-haskell" ];
    };

    langs-latex-internal = {
      auto-enable = builtins.elem "latex" cfg.langs.enable;
      packages = [ "org-special-block-extras" ];
    };

    langs-latex-lsp-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-latex-internal.enable;
      packages = [ "lsp-latex" ];
    };

    langs-rust-internal = {
      auto-enable = builtins.elem "rust" cfg.langs.enable;
      packages = [ "rustic" ];
    };

    langs-php-internal = {
      auto-enable = builtins.elem "php" cfg.langs.enable;
      packages = [ "php-mode" ];
    };

    langs-go-internal = {
      auto-enable = builtins.elem "go" cfg.langs.enable;
      packages = [ "go-mode" ];
    };

    langs-kotlin-internal = {
      auto-enable = builtins.elem "kotlin" cfg.langs.enable;
      packages = [ "kotlin-mode" ];
    };

    langs-java-internal = {
      auto-enable = builtins.elem "java" cfg.langs.enable;
    };

    langs-java-lsp-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-java-internal.enable;
      packages = [ "lsp-java" ];
    };

    langs-solidity-internal = {
      auto-enable = builtins.elem "solidity" cfg.langs.enable;
      packages = [ "solidity-mode" ];
    };

    langs-solidity-lsp-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-solidity-internal.enable;
      packages = [ "solidity-flycheck" ];
    };

    langs-solidity-misc-internal = {
      auto-enable = cfg.misc.code.enable && cfg.misc.code.completion
        == "company" && cfg.bundles.langs-solidity-internal.enable;
      packages = [ "company-solidity" ];
    };

    proof-assist = { packages = [ "proof-general" "lean4-mode" ]; };

    proff-assist-company = {
      auto-enable = cfg.bundles.proof-assist.enable && cfg.misc.code.enable
        && cfg.misc.code.completion == "company";
      packages = [ "company-coq" ];
    };

    langs-julia-internal = {
      auto-enable = builtins.elem "julia" cfg.langs.enable;
      packages = [ "solidity-mode" ];
    };

    langs-julia-lsp-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-julia-internal.enable;
      packages = [ "lsp-julia" ];
    };

    web = { packages = [ "impatient-mode" "web-mode" ]; };

    rss = { packages = [ "elfeed" "elfeed-org" "elfeed-goodies" ]; };

    org-misc-internal = {
      auto-enable = true;
      packages = [ "ob-mermaid" "ox-gfm" ];
    };

    org-extra-internal = {
      auto-enable = cfg.org.extra.enable;
      packages = [ "ox-json" "org-bullets" ];
    };

    org-reveal-internal = {
      auto-enable = cfg.org.reveal.enable;
      packages = [ "ox-reveal" ];
    };

    dicts = { };

    exwm = {
      packages = [ "exwm" ];
      config = { xsession = { enable = true; }; };
    };

    wayland = { };

    code-stats = {
      packages = [ "code-stats" ];
      config = {
        home.file.".emacs.d/private.el".text = ''
          (setq code-stats-token "${
            secrets.code-stats-api-key.${config.custom.settings.code-stats-machine}
          }")
        '';
      };
    };
  };

in {
  options = {
    custom.editors.emacs = {
      packages = makeEnableOptions (attrNames allPackages);
      bundles = makeEnableOptions (attrNames bundles);

      enable = mkOption {
        default = false;
        description = ''
          Emacs
        '';
      };

      useAsVisual = mkOption { default = false; };

      package = mkOption { default = pkgs.emacs29-pgtk; };

      server = mkOption {
        default = false;
        description = ''
          Emacs server
        '';
      };

      misc = {
        enable = mkOption { default = false; };

        completion = mkOption {
          default = "vertico";
          type = types.enum [ "vertico" "ivy" ];
        };

        code = {
          enable = mkOption { default = false; };
          completion = mkOption {
            default = "corfu";
            type = types.enum [ "company" "corfu" ];
          };

          auto-parens.enable = mkOption { default = false; };
        };
      };

      langs = {
        enable = mkOption {
          default = [ "misc" ];
          type = types.listOf (types.enum [
            "nix"
            "python"
            "rust"
            "typescript"
            "cpp"
            "haskell"
            "latex"
            "go"
            "kotlin"
            "java"
            "solidity"
            "julia"
            "misc"
            "lua"
          ]);
        };

        cpp.ls = mkOption {
          default = "ccls";
          type = types.enum [ "ccls" "clangd" ];
        };

        python.ls = mkOption {
          default = "pylsp";
          type = types.enum [ "pylsp" "pyright" ];
        };
      };

      code-assist = {
        enable = mkOption { default = false; };

        pretty.enable = mkOption { default = false; };
      };

      org = {
        style = mkOption { default = null; };

        roam = {
          enable = mkOption { default = false; };

          ui = mkOption { default = false; };
        };

        extra = { enable = mkOption { default = false; }; };

        reveal = { enable = mkOption { default = false; }; };
      };

      evil = {
        enable = mkOption { default = false; };

        extra = mkOption { default = true; };
      };

      pretty = {
        theme = mkOption { default = null; };
        extra = { enable = mkOption { default = false; }; };
        font-size = mkOption { default = 95; };
      };

      extraConfig = mkOption {
        default = ''
          ;; Extra config
        '';
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkMerge (map (name:
      let bundle = { auto-enable = false; } // bundles.${name};
      in { custom.editors.emacs.bundles.${name}.enable = bundle.auto-enable; })
      (attrNames bundles)))

    (mkMerge (map (name:
      let
        bundleDefault = {
          auto-enable = false;
          packages = [ ];
          config = { };
        };
        bundle = bundleDefault // bundles.${name};
        enabled = cfg.bundles.${name}.enable;
        val = if enabled then "t" else "nil";
      in mkMerge [
        ({
          home.file.".emacs.d/nixcfg.el".text = ''
            (setq nixcfg-bundle-${name} ${val})
          '';
        })
        (mkIf enabled (enablePackages bundle.packages))
        (mkIf enabled (bundle.config))
      ]) (attrNames bundles)))

    (mkMerge (map (pkg:
      let
        enabled = cfg.packages.${pkg}.enable;
        val = if enabled then "t" else "nil";
      in {
        home.file.".emacs.d/nixcfg.el".text = ''
          (setq nixcfg-${pkg} ${val})
        '';

        programs.emacs.extraPackages =
          if enabled then allPackages.${pkg} else _: [ ];
      }) (attrNames allPackages)))

    (mkIf cfg.useAsVisual { home.sessionVariables = { VISUAL = "emacs"; }; })

    {
      home.sessionVariables = {
        TEXMFDOTDIR = "\${TEXMFDOTDIR:-.}:~/.emacs.d/latex";
      };

      xdg.mimeApps = {
        defaultApplications = { "text/plain" = [ "emacsclient.desktop" ]; };
      };

      home.file.".emacs.d" = {
        source = ./.emacs.d;
        recursive = true;
      };

      home.file.".emacs.d/config.org" = {
        source = ./.emacs.d/config.org;
        onChange = ''
          [ -e ~/.emacs.d/config.el ] && rm ~/.emacs.d/config.el
        '';
      };

      home.file.".emacs.d/nixcfg.el".text = ''
        (setq nixcfg-font-size ${toString cfg.pretty.font-size})
      '';

      home.file.".emacs.d/extra.el".text = cfg.extraConfig;

      programs.emacs = {
        enable = true;
        package = cfg.package;
        overrides = import ./overrides.nix inputs;
        extraConfig = ''
          (setq comp-deferred-compilation t)
          (org-babel-load-file "~/.emacs.d/config.org")
        '';
      };

      home.packages = with pkgs; [ gsettings-desktop-schemas ];
    }

    (mkIf cfg.server {
      services.emacs = {
        enable = true;

        client = { enable = true; };
      };
    })

  ]);
}
