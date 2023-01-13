{ config, lib, pkgs, secrets, ... }@inputs:

with lib;

let
  cfg = config.custom.editors.emacs;
in

{
  options = {
    custom.editors.emacs = {
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


      code-stats = mkOption {
        default = false;
      };

      misc = {
        enable = mkOption {
          default = false;
        };

        vertico = mkOption {
          default = true;
        };
      };

      evil = mkOption {
        default = false;
      };

      additional-motions = mkOption {
        default = false;
      };

      visual = mkOption {
        default = false;
      };

      code-misc = mkOption {
        default = false;
      };

      prog-misc = mkOption {
        default = false;
      };

      web = {
        misc = mkOption {
          default = false;
        };

        dap = mkOption {
          default = false;
        };
      };

      ts-js = mkOption {
        default = false;
      };

      lsp = mkOption {
        default = false;
      };

      cpp = mkOption {
        default = false;
      };

      haskell = mkOption {
        default = false;
      };

      python = mkOption {
        default = false;
      };

      latex = mkOption {
        default = false;
      };

      rust = mkOption {
        default = false;
      };

      go = mkOption {
        default = false;
      };

      kotlin = mkOption {
        default = false;
      };

      java = mkOption {
        default = false;
      };

      org-additional = mkOption {
        default = false;
      };

      roam = mkOption {
        default = false;
      };

      rss = mkOption {
        default = false;
      };

      nix = mkOption {
        default = false;
      };

      solidity = mkOption {
        default = false;
      };

      proof-assist = mkOption {
        default = false;
      };

      julia = mkOption {
        default = false;
      };

      exwm = mkOption {
        default = false;
      };

      dicts = mkOption {
        default = true;
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
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

      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-modules t)
      '';

      programs.emacs = {
        enable = true;
        package = pkgs.emacsNativeComp;
        overrides = import ./overrides.nix inputs;
        extraPackages = epkgs: with epkgs; [
          use-package
          gcmh
          general
          which-key
          direnv
        ];
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

    (mkIf cfg.code-stats {
      home.file.".emacs.d/private.el".text = ''
        (setq code-stats-token "${secrets.code-stats-api-key}")
      '';

      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-code-stats t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        code-stats
      ];
    })

    (mkIf cfg.misc.enable {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-misc t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        projectile
        magit
        treemacs
        counsel
        counsel-projectile
        treemacs-projectile
        undo-tree
        minimap
        esup
      ];
    })

    (mkIf (cfg.misc.enable && cfg.misc.vertico) {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-vertico t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        vertico
        consult
        consult-projectile
        marginalia
        orderless
      ];
    })

    (mkIf (cfg.misc.enable && !cfg.misc.vertico) {
      programs.emacs.extraPackages = epkgs: with epkgs; [
        counsel
        counsel-projectile
      ];
    })


    (mkIf cfg.evil {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-evil t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        goto-chg
        evil
        evil-collection
        evil-snipe
        evil-surround
        evil-multiedit
        evil-mc
      ];
    })

    (mkIf cfg.additional-motions {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-additional-motions t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        avy
        ace-window
        hydra
        emacs-everywhere
        minimap
      ];

      home.packages = with pkgs; [
        xdotool
        xorg.xwininfo
      ];
    })

    (mkIf cfg.visual {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-visual t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        all-the-icons
        hl-todo
        rainbow-delimiters
        dashboard
        diff-hl
        doom-modeline
        doom-themes
        centaur-tabs
      ];
    })

    (mkIf cfg.code-misc {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-code-misc t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        smartparens
        editorconfig
        yasnippet
        yasnippet-snippets
        format-all
        company
      ];
    })

    (mkIf cfg.prog-misc {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-prog-misc t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        dockerfile-mode
        yaml-mode
        graphviz-dot-mode
        bison-mode
      ];
    })

    (mkIf cfg.web.misc {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-web-misc t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        impatient-mode
        web-mode
      ];
    })

    (mkIf cfg.web.dap {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-web-dap t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        dap-mode
      ];
    })

    (mkIf cfg.ts-js {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-ts-js t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        tide
        rjsx-mode
        typescript-mode
      ];
    })

    (mkIf cfg.lsp {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-lsp t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        lsp-mode
        lsp-ui
        flycheck
      ];
    })

    (mkIf cfg.cpp {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-cpp t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        ccls
      ];

      home.packages = with pkgs; [
        ccls
      ];
    })

    (mkIf cfg.haskell {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-haskell t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        haskell-mode
      ];
    })

    (mkIf (cfg.haskell && cfg.lsp) {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-haskell t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        lsp-haskell
      ];
    })

    (mkIf cfg.python {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-python t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        anaconda-mode
        company-anaconda
      ];
    })

    (mkIf (cfg.python && cfg.lsp) {
      programs.emacs.extraPackages = epkgs: with epkgs; [
        lsp-pyright
      ];
    })

    (mkIf cfg.latex {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-latex t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        org-special-block-extras
      ];
    })

    (mkIf (cfg.latex && cfg.lsp) {
      programs.emacs.extraPackages = epkgs: with epkgs; [
        lsp-latex
      ];
    })

    (mkIf cfg.rust {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-rust t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        rustic
      ];
    })

    (mkIf cfg.go {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-go t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        go-mode
      ];
    })

    (mkIf cfg.kotlin {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-kotlin t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        kotlin-mode
      ];
    })

    (mkIf cfg.java {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-java t)
      '';
    })

    (mkIf (cfg.java && cfg.lsp) {
      programs.emacs.extraPackages = epkgs: with epkgs; [
        lsp-java
      ];
    })

    (mkIf cfg.org-additional {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-org-additional t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        org-special-block-extras
        ox-reveal
        ox-json
        org-bullets
        org-special-block-extras
      ];
    })

    (mkIf cfg.roam {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-roam t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        org-roam
        org-roam-ui
        websocket
      ];
    })

    (mkIf cfg.rss {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-rss t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        elfeed
        elfeed-org
        elfeed-goodies
      ];
    })

    (mkIf cfg.nix {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-nix t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        nix-mode
        nixos-options
        company-nixos-options
        nix-sandbox
      ];

      home.packages = with pkgs; [
        rnix-lsp
      ];
    })

    (mkIf cfg.solidity {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-solidity t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        solidity-mode
        solidity-flycheck
        company-solidity
      ];
    })

    (mkIf cfg.proof-assist {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-proof-assist t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        proof-general
        company-coq
      ];
    })

    (mkIf cfg.julia {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-julia t)
      '';

      programs.emacs.extraPackages = epkgs: with epkgs; [
        julia-mode
      ];
    })

    (mkIf (cfg.julia && cfg.lsp) {
      programs.emacs.extraPackages = epkgs: with epkgs; [
        (lsp-julia.overrideAttrs (old: {
          patches = [
            ./lsp-julia.patch
          ];
        }))
      ];
    })

    (mkIf (cfg.misc.enable && cfg.evil) {
      programs.emacs.extraPackages = epkgs: with epkgs; [
        treemacs-evil
      ];
    })

    (mkIf (cfg.misc.enable && cfg.lsp) {
      programs.emacs.extraPackages = epkgs: with epkgs; [
        lsp-treemacs
      ];
    })

    (mkIf (cfg.misc.enable && !cfg.misc.vertico && cfg.lsp) {
      programs.emacs.extraPackages = epkgs: with epkgs; [
        lsp-ivy
      ];
    })

    (mkIf cfg.exwm {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-exwm t)
      '';

      xsession = {
        enable = true;
      };


      programs.emacs.extraPackages = epkgs: with epkgs; [
        exwm
      ];
    })

    (mkIf cfg.dicts {
      home.file.".emacs.d/nix-modules.el".text = ''
        (setq nix-dicts t)
      '';
    })
  ]);
}
