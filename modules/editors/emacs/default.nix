{ config, pkgs, secrets, ...}@inputs:

{ home.file.".emacs.d" = {
    source = ./.emacs.d;
    recursive = true;
  };

  home.file.".emacs.d/private.el".text = '' (setq code-stats-token "${secrets.code-stats-api-key}") '';

  home.file.".emacs.d/config.org" = {
    source = ./.emacs.d/config.org;
    onChange = ''
      [ -e ~/.emacs.d/config.el] && rm ~/.emacs.d/config.el
    '';
  };

  home.file.".emacs.d/nix.el".text = ''
                                   (setq lsp-clangd-binary-path "${pkgs.clang-tools}/bin/clangd")
  '';

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
    overrides = import ./overrides.nix inputs;
    extraPackages = epkgs: with epkgs; [
      gcmh
      projectile
      counsel-projectile
      magit
      treemacs
      treemacs-evil
      treemacs-projectile
      undo-tree
      goto-chg
      evil
      evil-collection
      evil-snipe
      evil-surround
      evil-multiedit
      evil-mc
      all-the-icons
      hl-todo
      rainbow-delimiters
      avy
      ace-window
      dashboard
      diff-hl
      doom-modeline
      ewal
      ewal-doom-themes
      doom-themes
      general
      hydra
      which-key
      code-stats
      smartparens
      editorconfig
      yasnippet
      yasnippet-snippets
      format-all
      dockerfile-mode
      impatient-mode
      web-mode
      company
      # company-lsp
      counsel
      lsp-mode
      flycheck
      lsp-treemacs
      lsp-ivy
      ccls
      lsp-haskell
      lsp-pyright
      anaconda-mode
      company-anaconda
      ein
      lsp-latex
      rustic
      go-mode
      haskell-mode
      yaml-mode
      kotlin-mode
      graphviz-dot-mode
      ob-ipython
      lsp-java
      org-special-block-extras
      ox-reveal
      ox-json
      org-bullets
      org-roam
      websocket
      elfeed
      elfeed-org
      elfeed-goodies
      nix-mode
      nixos-options
      company-nixos-options
      nix-sandbox
      direnv
      lsp-pyright

      org-roam-ui
    ];
  };

  home.packages = with pkgs; [
    rnix-lsp
  ];

  services.emacs = {
    enable = true;

    client = {
      enable = true;
    };
  };

}
