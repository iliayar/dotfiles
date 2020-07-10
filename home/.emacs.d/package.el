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
  :config
  (evil-mode 1))

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

;; Editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
