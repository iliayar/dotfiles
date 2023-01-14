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
    "use-package"
    "gcmh"
    "general"
    "which-key"
    "direnv"
    "code-stats"
    "projectile"
    "magit"
    "treemacs"
    "counsel"
    "counsel-projectile"
    "treemacs-projectile"
    "undo-tree"
    "minimap"
    "esup"
    "vertico"
    "consult"
    "consult-projectile"
    "marginalia"
    "orderless"
    "counsel"
    "goto-chg"
    "evil"
    "evil-collection"
    "evil-snipe"
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
    "doom-modeline"
    "doom-themes"
    "centaur-tabs"
    "smartparens"
    "editorconfig"
    "yasnippet"
    "yasnippet-snippets"
    "format-all"
    "company"
    "dockerfile-mode"
    "yaml-mode"
    "graphviz-dot-mode"
    "bison-mode"
    "impatient-mode"
    "web-mode"
    "dap-mode"
    "tide"
    "rjsx-mode"
    "typescript-mode"
    "lsp-mode"
    "lsp-ui"
    "flycheck"
    "ccls"
    "haskell-mode"
    "lsp-haskell"
    "anaconda-mode"
    "company-anaconda"
    "lsp-pyright"
    "org-special-block-extras"
    "lsp-latex"
    "rustic"
    "go-mode"
    "kotlin-mode"
    "lsp-java"
    "org-special-block-extras"
    "ox-reveal"
    "ox-json"
    "org-bullets"
    "org-special-block-extras"
    "org-roam"
    "org-roam-ui"
    "websocket"
    "elfeed"
    "elfeed-org"
    "elfeed-goodies"
    "nix-mode"
    "nixos-options"
    "company-nixos-options"
    "nix-sandbox"
    "solidity-mode"
    "solidity-flycheck"
    "company-solidity"
    "proof-general"
    "company-coq"
    "julia-mode"
    "treemacs-evil"
    "lsp-treemacs"
    "lsp-ivy"
    "exwm"
  ] // {
    "lsp-julia" = epkgs:
      [
        (epkgs.lsp-julia.overrideAttrs
          (old: { patches = [ ./lsp-julia.patch ]; }))
      ];
  };

  bundles = {
    basic = {
      auto-enable = true;
      packages = [ "use-package" "gcmh" "general" "hydra" "direnv" ];
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
        "treemacs"
        "treemacs-projectile"
        "minimap"
        "esup"
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
        "smartparens"
        "editorconfig"
        "yasnippet"
        "yasnippet-snippets"
        "format-all"
        "company"
      ];
    };

    langs-nix-internal = {
      auto-enable = builtins.elem "nix" cfg.langs;
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
      config = { home.packages = [ pkgs.nixfmt ]; };
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
      packages = [ "treemacs-evil" ];
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
      packages = [ "dashboard" "diff-hl" "doom-modeline" "centaur-tabs" ];
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

    langs-python-internal = { auto-enable = builtins.elem "python" cfg.langs; };

    langs-python-lsp-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-python-internal.enable;
      config = {
        custom.dev.python.additionalPackages = pypkgs:
          [ pypkgs.python-lsp-server ];
      };
    };

    langs-misc-internal = {
      auto-enable = builtins.elem "misc" cfg.langs;
      packages =
        [ "dockerfile-mode" "yaml-mode" "graphviz-dot-mode" "bison-mode" ];
    };

    langs-typescript-internal = {
      auto-enable = builins.elem "typescript" cfg.langs;
      packages = [ "tide" "rjsx-mode" "typescript-mode" ];
    };

    langs-cpp-internal = { auto-enable = builtins.elem "cpp" cfg.langs; };

    langs-cpp-lsp-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-cpp-internal.enable;
      packages = [ "ccls" ];
      config = { home.packages = [ pkgs.ccls ]; };
    };

    langs-haskell-internal = {
      auto-enable = builtins.elem "haskell" cfg.langs;
      packages = [ "haskell-mode" ];
    };

    langs-haskell-lsp-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-haskell-internal.enable;
      packages = [ "lsp-haskell" ];
    };

    langs-latex-internal = {
      auto-enable = builtins.elem "latex" cfg.langs;
      packages = [ "org-special-block-extras" ];
    };

    langs-latex-lsp-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-latex-internal.enable;
      packages = [ "lsp-latex" ];
    };

    langs-rust-internal = {
      auto-enable = builtins.elem "rust" cfg.langs;
      packages = [ "rustic" ];
    };

    langs-go-internal = {
      auto-enable = builtins.elem "go" cfg.langs;
      packages = [ "go-mode" ];
    };

    langs-kotlin-internal = {
      auto-enable = builtins.elem "kotlin" cfg.langs;
      packages = [ "kotlin-mode" ];
    };

    langs-java-internal = { auto-enable = builtins.elem "java" cfg.langs; };

    langs-java-lsp-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-java-internal.enable;
      packages = [ "lsp-java" ];
    };

    langs-solidity-internal = {
      auto-enable = builtins.elem "solidity" cfg.langs;
      packages = [ "solidity-mode" ];
    };

    langs-solidity-lsp-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-solidity-internal.enable;
      packages = [ "solidity-flycheck" ];
    };

    langs-solidity-misc-internal = {
      auto-enable = cfg.misc.code.enable
        && cfg.bundles.langs-solidity-internal.enable;
      packages = [ "company-solidity" ];
    };

    proof-assist = { packages = [ "proof-general" "company-coq" ]; };

    langs-julia-internal = {
      auto-enable = builtins.elem "julia" cfg.langs;
      packages = [ "solidity-mode" ];
    };

    langs-julia-lsp-internal = {
      auto-enable = cfg.bundles.lsp-internal.enable
        && cfg.bundles.langs-julia-internal.enable;
      packages = [ "lsp-julia" ];
    };

    web = { packages = [ "impatient-mode" "web-mode" ]; };

    rss = { packages = [ "elfeed" "elfeed-org" "elfeed-goodies" ]; };

    org-extra-internal = {
      auto-enable = cfg.org.extra.enable;
      packages = [ "ox-reveal" "ox-json" "org-bullets" ];
    };

    dicts = { };

    exwm = {
      packages = [ "exwm" ];
      config = { xsession = { enable = true; }; };
    };

    code-stats = {
      packages = [ "code-stats" ];
      config = {
        home.file.".emacs.d/private.el".text = ''
          (setq code-stats-token "${secrets.code-stats-api-key}")
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

        code = { enable = mkOption { default = false; }; };
      };

      langs = mkOption {
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
        ]);
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
      };

      evil = {
        enable = mkOption { default = false; };

        extra = mkOption { default = true; };
      };

      pretty = {
        theme = mkOption { default = null; };
        extra = { enable = mkOption { default = false; }; };
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

    {
      home.sessionVariables = { VISUAL = "emacs"; };

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

      programs.emacs = {
        enable = true;
        package = pkgs.emacsNativeComp;
        overrides = import ./overrides.nix inputs;
      };
    }

    (mkIf cfg.server {
      services.emacs = {
        enable = true;

        client = { enable = true; };
      };
    })

  ]);
}
