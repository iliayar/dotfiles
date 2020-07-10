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

;; Interactive M-x search
(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  ;(global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1))

;; Managing project
(use-package projectile
  :ensure t
  :config
  (projectile-mode))
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

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
