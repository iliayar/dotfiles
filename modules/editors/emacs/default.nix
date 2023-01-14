{ config, lib, pkgs, secrets, ... }@inputs:

with lib;

let
  cfg = config.custom.editors.emacs;

  makeEnableOption = name: {
    "${name}".enable = mkOption {
      default = false;
    };
  };

  makePackagesDictId = foldl (acc: e: acc // { "${e}" = epkgs: [ epkgs.${e} ]; }) {};

  makeEnableOptions =
    foldl (acc: name: acc // (makeEnableOption name)) {};

  enablePackages = pkgs: mkMerge (map (pkg: {
    custom.editors.emacs.packages.${pkg}.enable = true;
  }) pkgs);

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
    "lsp-julia" = epkgs: [
      (epkgs.lsp-julia.overrideAttrs (old: {
        patches = [
          ./lsp-julia.patch
        ];
      }))
    ];
  };

  # TODO package:
  # "which-key" 
  # "undo-tree"
  # "goto-chg"
  # "evil-collection"
  # "evil-snipe"
  # "evil-multiedit"
  # "treemacs-evil"

  bundles = {
    # Example:
    # julia-lsp = {
    #   auto-enable = cfg.bundles.lsp.enable && cfg.bundles.julia.enable;
    #   packages = [ "lsp-julia" ];
    # };

    basic = {
      auto-enable = true;
      packages = [
        "use-package"
        "gcmh"
        "general"
        "hydra"
      ];
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
      ];
    };

    vertico-internal = {
      auto-enable = cfg.misc.enable && cfg.misc.completion == "vertico";
      packages = [
        "vertico"
        "consult"
        "consult-projectile"
        "marginalia"
        "orderless"
      ];
    };

    ivy-internal = {
      auto-enable = cfg.misc.enable && cfg.misc.completion == "ivy";
      packages = [
        "counsel"
        "counsel-projectile"
      ];
    };

    org-style-v1 = {
      auto-enable = cfg.org.style == "v1";
    };

    org-roam-internal = {
      auto-enable = cfg.org.roam.enable;
      packages = [ "org-roam" ];
    };

    org-roam-ui-internal = {
      auto-enable = cfg.org.roam.ui;
      packages = [
        "org-roam-ui"
        "websocket"
      ];
    };

    evil-internal = {
      auto-enable = cfg.evil.enable;
      packages = [ "evil" ];
    };

    evil-extra-internal = {
      auto-enable = cfg.evil.enable && cfg.evil.extra;
      packages = [
        "evil-mc" 
        "evil-surround"
      ];
    };

    theme-internal = {
      auto-enable = cfg.pretty.theme != null;
      packages = [
        "doom-themes"
      ];
      config = {
        home.file.".emacs.d/nixcfg.el".text = ''
            (setq nixcfg-theme '${cfg.pretty.theme})
          '';
      };
    };

    dicts = {};
    latex = {};

    exwm = {
      packages = [ "exwm" ];
      config = {
        xsession = {
          enable = true;
        };
      };
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
in

{
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
        enable = mkOption {
          default = false;
        };

        completion = mkOption {
          default = "vertico";
          type = types.enum [ "vertico" "ivy" ];
        };
      };

      org = {
        style = mkOption {
          default = null;
        };

        roam = {
          enable = mkOption {
            default = false;
          };

          ui = mkOption {
            default = false;
          };
        };
      };

      evil = {
        enable = mkOption {
          default = false;
        };

        extra = mkOption {
          default = true;
        };
      };

      pretty = {
        theme = mkOption {
          default = null;
        };
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkMerge (map (name:
      let
        bundleDefault = {
          auto-enable = false;
          packages = [];
          config = {};
        };
        bundle = bundleDefault // bundles.${name};
        enabled = cfg.bundles.${name}.enable || bundle.auto-enable;
        val = if enabled then "t" else "nil";
      in mkMerge [
        ({
          home.file.".emacs.d/nixcfg.el".text = ''
            (setq nixcfg-bundle-${name} ${val})
          '';
        })
        (mkIf enabled (enablePackages bundle.packages))
        (mkIf enabled (bundle.config))
      ]
    ) (attrNames bundles)))

    (mkMerge (map (pkg:
      let
        enabled = cfg.packages.${pkg}.enable;
        val = if enabled then "t" else "nil";
      in {
        home.file.".emacs.d/nixcfg.el".text = ''
          (setq nixcfg-${pkg} ${val})
        '';

        programs.emacs.extraPackages = if enabled then allPackages.${pkg} else _: [];
      }
    ) (attrNames allPackages)))

    {
      home.sessionVariables = {
        VISUAL = "emacs";
      };

      xdg.mimeApps = {
        defaultApplications = {
          "text/plain" = [ "emacsclient.desktop" ];
        };
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

        client = {
          enable = true;
        };
      };
    })

    # (mkIf cfg.additional-motions {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-additional-motions t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     avy
    #     ace-window
    #     hydra
    #     emacs-everywhere
    #     minimap
    #   ];

    #   home.packages = with pkgs; [
    #     xdotool
    #     xorg.xwininfo
    #   ];
    # })

    # (mkIf cfg.visual {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-visual t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     all-the-icons
    #     hl-todo
    #     rainbow-delimiters
    #     dashboard
    #     diff-hl
    #     doom-modeline
    #     centaur-tabs
    #   ];
    # })

    # (mkIf cfg.code-misc {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-code-misc t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     smartparens
    #     editorconfig
    #     yasnippet
    #     yasnippet-snippets
    #     format-all
    #     company
    #   ];
    # })

    # (mkIf cfg.prog-misc {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-prog-misc t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     dockerfile-mode
    #     yaml-mode
    #     graphviz-dot-mode
    #     bison-mode
    #   ];
    # })

    # (mkIf cfg.web.misc {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-web-misc t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     impatient-mode
    #     web-mode
    #   ];
    # })

    # (mkIf cfg.web.dap {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-web-dap t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     dap-mode
    #   ];
    # })

    # (mkIf cfg.ts-js {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-ts-js t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     tide
    #     rjsx-mode
    #     typescript-mode
    #   ];
    # })

    # (mkIf cfg.lsp {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-lsp t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     lsp-mode
    #     lsp-ui
    #     flycheck
    #   ];
    # })

    # (mkIf cfg.cpp {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-cpp t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     ccls
    #   ];

    #   home.packages = with pkgs; [
    #     ccls
    #   ];
    # })

    # (mkIf cfg.haskell {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-haskell t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     haskell-mode
    #   ];
    # })

    # (mkIf (cfg.haskell && cfg.lsp) {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-haskell t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     lsp-haskell
    #   ];
    # })

    # (mkIf cfg.python {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-python t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     anaconda-mode
    #     company-anaconda
    #   ];
    # })

    # (mkIf (cfg.python && cfg.lsp) {
    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     lsp-pyright
    #   ];
    # })

    # (mkIf cfg.latex {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-latex t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     org-special-block-extras
    #   ];
    # })

    # (mkIf (cfg.latex && cfg.lsp) {
    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     lsp-latex
    #   ];
    # })

    # (mkIf cfg.rust {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-rust t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     rustic
    #   ];
    # })

    # (mkIf cfg.go {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-go t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     go-mode
    #   ];
    # })

    # (mkIf cfg.kotlin {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-kotlin t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     kotlin-mode
    #   ];
    # })

    # (mkIf cfg.java {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-java t)
    #   '';
    # })

    # (mkIf (cfg.java && cfg.lsp) {
    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     lsp-java
    #   ];
    # })

    # (mkIf cfg.org-additional {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-org-additional t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     org-special-block-extras
    #     ox-reveal
    #     ox-json
    #     org-bullets
    #     org-special-block-extras
    #   ];
    # })

    # (mkIf cfg.rss {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-rss t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     elfeed
    #     elfeed-org
    #     elfeed-goodies
    #   ];
    # })

    # (mkIf cfg.nix {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-nix t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     nix-mode
    #     nixos-options
    #     company-nixos-options
    #     nix-sandbox
    #   ];

    #   home.packages = with pkgs; [
    #     rnix-lsp
    #   ];
    # })

    # (mkIf cfg.solidity {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-solidity t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     solidity-mode
    #     solidity-flycheck
    #     company-solidity
    #   ];
    # })

    # (mkIf cfg.proof-assist {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-proof-assist t)
    #   '';

    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     proof-general
    #     company-coq
    #   ];
    # })

    # # (mkIf cfg.julia {
    # #   home.file.".emacs.d/nix-modules.el".text = ''
    # #     (setq nix-julia t)
    # #   '';

    # #   programs.emacs.extraPackages = epkgs: with epkgs; [
    # #     julia-mode
    # #   ];
    # # })

    # # (mkIf (cfg.julia && cfg.lsp) {
    # #   programs.emacs.extraPackages = epkgs: with epkgs; [
    # #     (lsp-julia.overrideAttrs (old: {
    # #       patches = [
    # #         ./lsp-julia.patch
    # #       ];
    # #     }))
    # #   ];
    # # })

    # (mkIf (cfg.misc.enable && cfg.lsp) {
    #   programs.emacs.extraPackages = epkgs: with epkgs; [
    #     lsp-treemacs
    #   ];
    # })

    # # (mkIf (cfg.misc.enable && !cfg.misc.vertico && cfg.lsp) {
    # #   programs.emacs.extraPackages = epkgs: with epkgs; [
    # #     lsp-ivy
    # #   ];
    # # })

    # (mkIf cfg.dicts {
    #   home.file.".emacs.d/nix-modules.el".text = ''
    #     (setq nix-dicts t)
    #   '';
    # })
  ]);
}
