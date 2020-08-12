;; Auto completition
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t))

(use-package company-box
  :ensure t)

(use-package counsel
  :ensure t
  :init
  (ivy-mode 1))

;; Managing project
(use-package projectile
  :ensure t
  :config
  (projectile-mode))
(use-package counsel-projectile
  :ensure t)

;; Git integration
(use-package magit
  :ensure t)

;; Hightlight TODO words
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode t))

;; Key hints
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

;; Evil mode with dependecies
(use-package undo-tree
  :ensure t)
(use-package goto-chg
  :ensure t)
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
(use-package evil-snipe
  :ensure t
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

;; Smart parenthesis
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode))

;; Theme
(use-package doom-themes
  :ensure t
  :preface (defvar region-fg nil) ; this prevents a weird bug with doom themes
  :init (load-theme 'doom-gruvbox t))

;; Treemacs
(use-package treemacs
  :ensure t)
(use-package treemacs-evil
  :ensure t)
(use-package treemacs-projectile
  :ensure t)

;; Startup Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; Icons
(use-package all-the-icons
 :ensure t)

;; Status Line
(use-package doom-modeline
  :ensure t
  :init 
  (doom-modeline-mode 1))

;; Rainbow paretheses
(use-package rainbow-delimiters
  :ensure t)

;; Vim surround like
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Multicursor
(use-package evil-multiedit
  :ensure t)
(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode 1))

;; Editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Snippets
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)


;; Latex
;(use-package auctex
  ;:ensure t)
;(use-package company-auctex
  ;:ensure t)

;; LSP
(use-package  lsp-mode
  :hook (
        ;; (XXX-mode . lsp) ;; auto enable lsp on XXX-mode
        (lsp-mode . lsp-enable-which-key-integration) 
        )
  :ensure t)
(use-package flycheck
  :ensure t)
(use-package lsp-ui
  :ensure t)
(use-package lsp-treemacs
  :ensure t)
(use-package lsp-ivy
  :ensure t)

;; Go
(use-package go-mode
  :ensure t)

;; Haskell
(use-package haskell-mode
  :ensure t)

;; Install https://github.com/latex-lsp/texlab.git before
(use-package lsp-latex
  :ensure t)

;; Yaml
(use-package yaml-mode
  :ensure t)

;; Org mode
(use-package org-bullets
  :ensure t)

;; Avy search
(use-package avy
  :ensure t)

;; Window managment
(use-package ace-window
  :ensure t)

;; Keybindings
(use-package general
  :ensure t)
(use-package hydra
  :ensure t)
