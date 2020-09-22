(require 'package)
(add-to-list
 'package-archives
 ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
 '("melpa" . "https://melpa.org/packages/")
 t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq evil-want-keybinding nil)

;(setq evil-search-module 'evil-search)

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t))

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-enable-snippet t)
  (setq lsp-enable-snippet t))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :config
  (push '(company-yasnippet . (:all "lime green" :selected
    (:background "black"))) company-box-backends-colors))

;; (use-package company-fuzzy
;;   :ensure t
;;   :config
;;   (global-company-fuzzy-mode f))

(use-package counsel
  :ensure t
  :init
  (ivy-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package impatient-mode
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode))
(use-package counsel-projectile
  :ensure t)

(use-package magit
  :ensure t)

(use-package treemacs
  :ensure t)
(use-package treemacs-evil
  :ensure t)
(use-package treemacs-projectile
  :ensure t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package undo-tree
  :ensure t)
(use-package goto-chg
  :ensure t)
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
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

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-multiedit
  :ensure t)
(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode 1))

(use-package all-the-icons
 :ensure t)

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode t))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package org-bullets
  :ensure t)

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)

(use-package format-all
  :ensure t)

(use-package avy
  :ensure t)

(use-package ace-window
  :ensure t)

(use-package  lsp-mode
  :hook (
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

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms))))
(use-package anaconda-mode
  :ensure t)
(use-package company-anaconda
  :ensure t)

(use-package lsp-latex
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package kotlin-mode
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init 
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-icon t))

(use-package ewal
  :ensure t
  :init (setq ewal-use-built-in-always nil
              ewal-use-built-in-on-failure-p t
              ewal-built-in-palette "doom-gruvbox"))
(use-package ewal-doom-themes
  :ensure t)
  ;; :init (load-theme 'ewal-doom-one t))

;; (use-package xresources-theme
;;   :ensure t)

(use-package doom-themes
  :ensure t
  :preface (defvar region-fg nil)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  :init (load-theme 'doom-monokai-classic t))

(use-package general
  :ensure t)

(use-package hydra
  :ensure t)

(setq company-backends 
  '(company-capf 
    company-bbdb 
    company-clang 
    company-keywords 
    company-yasnippet 
    company-lsp 
    company-files 
    company-anaconda))

(defun mars/company-backend-with-yas (backends)
  "Add :with company-yasnippet to company BACKENDS.
Taken from https://github.com/syl20bnr/spacemacs/pull/179."
  (if (and (listp backends) (memq 'company-yasnippet backends))
    backends
    (append (if (consp backends)
              backends
              (list backends))
      '(:with company-yasnippet))))

(defun add-yas-in-company ()
  (setq company-backends
    (mapcar #'mars/company-backend-with-yas company-backends)))

(add-yas-in-company)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(setq org-file-apps
      (append '(
                ("\\.pdf\\'" . "zathura %s")
                ) org-file-apps ))

(defun kill-buffer-if-exists (buffer)
  (when (not (eq nil (get-buffer buffer)))
     (delete-windows-on buffer) (kill-buffer buffer)))

(defun kill-compilation-buffer ()
  (interactive)
  (kill-buffer-if-exists "*compilation*"))
  

(defun my-compile ()
      "Run compile and resize the compile window"
      (interactive)
      (progn
        (call-interactively 'compile)
        (setq cur (selected-window))
        (setq w (get-buffer-window "*compilation*"))
        (select-window w)
        (setq h (window-height w))
        ;; (shrink-window (- h 20))
        (select-window cur)))

(defun my-compilation-hook () 
    "Make sure that the compile window is splitting vertically"
    (progn
      (if (not (get-buffer-window "*compilation*"))
         (progn
	    (split-window-vertically)))))

;; (add-hook 'compilation-mode-hook 'my-compilation-hook)
;; (remove-hook 'compilation-mode-hook 'my-compilation-hook t)

(setq compilation-scroll-output 'first-error)

(defun init-hooks () (global-display-line-numbers-mode 1))

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-center-content t)
(setq dashboard-startup-banner "~/Themes/Neofetch.png")
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(setq dashboard-items '((recents  . 5)
                        ;(bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

(add-hook 'after-init-hook 'init-hooks)

(add-hook 'shell-mode-hook (lambda () (company-mode nil)))

(scroll-bar-mode 0) ; no scroll bar
(tool-bar-mode 0) ; no tool bar
(menu-bar-mode 0) ; no menu bar
(show-paren-mode 1) ; visualize matching parenthesees
(global-hl-line-mode 1) ; highlight current line
(eldoc-mode 1) ; enable docs in minibuffer
;; (setq inhibit-startup-screen 1) ; no start screen

(setq ivy-use-selectable-prompt t)

;; store all backups in a single directory 
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; y or n instead of yes-or no
(fset 'yes-or-no-p 'y-or-n-p)

;; no annoying bell!
(setq ring-bell-function 'ignore)

;; set font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 85)

(setq company-math-allow-latex-symbols-in-faces t)

;; isearch
(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
(setq case-fold-search t)

(setq projectile-completion-system 'ivy)

;; Org-mode
(setq org-hide-emphaisi-markers t)
(add-hook 'org-mode-hook 
          (lambda () 
            (org-bullets-mode 1)
            (org-indent-mode 1)))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-default-notes-file (concat org-directory "/Notes.org"))

(eval-after-load "org"
  '(require 'ox-md nil t))

(setq org-todo-keywords
      '((sequence "TODO" "FIXME" "|" "DONE" )))

(setq ivy-initial-inputs-alist nil)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; set my init filt to be this file
(setq user-init-file "~/.emacs.d/init.el")

;; Evil initial states
(cl-loop for (mode . state) in '( (dired-mode . emacs)
                             )
      do (evil-set-initial-state mode state))

;; Highlight TODO colors
(setq hl-todo-keyword-faces
      '(("TODO"   . "#fabd2f")
        ("FIXME"  . "#fb4934")))

(general-define-key
  :keymaps 'company-active-map
  "<tab>"     'yas-expand
  "<backtab>" 'company-complete-selection)

(general-define-key
  "M-x" 'counsel-M-x)

(general-define-key
  :states '(normal visual emacs insert treemacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :keymaps 'override
  "bb" 'ibuffer
  "cd" 'kill-compilation-buffer
  "cc" 'compile
  "cr" 'lsp-rename
  "sl" 'lsp
  "ss" 'lsp-workspace-shutdown
  "sr" 'lsp-workspace-restart
  "wk" 'kill-buffer-and-window
  "wd" 'delete-window
  "ww" 'ace-window
  "gs" 'avy-goto-char-timer
  "gl" 'avy-goto-line
  "wr" 'hydra-window-resize-menu/body
  "pp" 'projectile-switch-project
  "pf" 'counsel-projectile-find-file
  "pc" 'projectile-compile-project
  "ff" 'counsel-find-file
  "cl" 'comment-or-uncomment-region
  "cf" 'counsel-grep-or-swiper
  "op" 'treemacs
  "om" 'magit)

(general-define-key
  :states '(visual)
  :keymaps 'override
  "R"  'evil-multiedit-match-all
  )

(general-define-key
  :states '(normal visual insert)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :keymaps 'latex-mode-map
  "si" 'latex-insert-block
  )
;; Hydra

(defhydra hydra-window-resize-menu (:color red
                                    :hint nil)
  "
  Window Resize
  -------------
       /\\
        _k_
  < _h_     _l_ >
        _j_
        v
  "
  ("h" evil-window-decrease-width)
  ("l" evil-window-increase-width)
  ("k" evil-window-decrease-height)
  ("j" evil-window-increase-height)
  ("c" nil "Cancel"))
