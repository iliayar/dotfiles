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
  (setq company-idle-delay 0.3)
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

(use-package web-mode
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

(use-package code-stats
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

(use-package dockerfile-mode
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

(use-package graphviz-dot-mode
  :ensure t)

(use-package ob-ipython
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

;; Org-mode
(setq org-hide-emphaisi-markers t)
(add-hook 'org-mode-hook 
          (lambda () 
            (org-bullets-mode 1)
            (org-indent-mode nil)
;; Uncomment for enabling auto preview LaTeX in org-mode
;;            (add-hook 'post-command-hook 'cw/org-auto-toggle-fragment-display t)
))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-default-notes-file (concat org-directory "/Notes.org"))

(eval-after-load "org"
(progn
  '(require 'ox-md nil t)
  '(require 'ox-latex nil t)))

(setq org-todo-keywords
      '((sequence "TODO" "FIXME" "|" "DONE" )))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(setq org-file-apps
      (append '(
                ("\\.pdf\\'" . "zathura %s")
                ) org-file-apps ))

(setq org-latex-packages-alist '(
      ("T1, T2A" "fontenc" t)
      ("lutf8" "luainputenc" t)
      ("russian, english" "babel" t)
      ("" "minted" t)
      ("" "graphicx" t)
      ("" "longtable" t)
      ("" "hyperref" t)
      ("" "natbib" t)
      ("" "amssymb" t)
      ("" "amsmath" t)
      ("" "grffile" t)
      ("" "wrapfig" t)
      ("" "rotating" t)
      ("" "placeins" t)
      ("normalem" "ulem" t)
      ("" "amsmath" t)
      ("" "textcomp" t)
      ("" "capt-of" t)))
  ;; Reset default value. For debugging
;;  (custom-reevaluate-setting 'org-latex-classes)
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
      '("general"
       "\\documentclass{article}
       [NO-DEFAULT-PACKAGES]
       [PACKAGES]
       [EXTRA]
       \\usepackage{geometry}
       \\geometry{a4paper,left=2.5cm,top=2cm,right=2.5cm,bottom=2cm,marginparsep=7pt, marginparwidth=.6in}
"

       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
)))
(setq org-latex-listings 'minted
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-minted-options
   '(("frame" "lines") ("linenos=true") ("mathescape")))
(add-to-list 'org-latex-minted-langs '(ipython "python"))

(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
  (gnuplot . t)
  (python . t)
  (ipython . t)))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(setq org-confirm-babel-evaluate nil)
(setq org-src-tab-acts-natively t)

;
;; 28.07.2017
;; Charles Wang
;;

(defun org--list-latex-overlays (&optional beg end)
  "List all Org LaTeX overlays in current buffer.
Limit to overlays between BEG and END when those are provided."
  (org-remove-if-not
   (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
   (overlays-in (or beg (point-min)) (or end (point-max)))))

;;;;;;; Tweaks for Org & org-latex ;;;;;;

(defvar cw/org-last-fragment nil
  "Holds the type and position of last valid fragment we were on. Format: (FRAGMENT_TYPE FRAGMENT_POINT_BEGIN)"
  )

(setq cw/org-valid-fragment-type
      '(latex-fragment
        latex-environment
        link))

(defun cw/org-curr-fragment ()
  "Returns the type and position of the current fragment available for preview inside org-mode. Returns nil at non-displayable fragments"
  (let* ((fr (org-element-context))
         (fr-type (car fr)))
    (when (memq fr-type cw/org-valid-fragment-type)
      (list fr-type
            (org-element-property :begin fr))))
  )

(defun cw/org-remove-fragment-overlay (fr)
  "Remove fragment overlay at fr"
  (let ((fr-type (nth 0 fr))
        (fr-begin (nth 1 fr)))
    (goto-char fr-begin)
    (cond ((or (eq 'latex-fragment fr-type)
               (eq 'latex-environment fr-type))
           (let ((ov (loop for ov in (org--list-latex-overlays)
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           return ov)))
             (when ov
               (delete-overlay ov))))
          ((eq 'link fr-type)
           nil;; delete image overlay here?
           ))
    ))

(defun cw/org-preview-fragment (fr)
  "Preview org fragment at fr"
  (let ((fr-type (nth 0 fr))
        (fr-begin (nth 1 fr)))
    (goto-char fr-begin)
    (cond ((or (eq 'latex-fragment fr-type) ;; latex stuffs
               (eq 'latex-environment fr-type))
           (when (cw/org-curr-fragment) (org-preview-latex-fragment))) ;; only toggle preview when we're in a valid region (for inserting in the front of a fragment)


          ((eq 'link fr-type) ;; for images
           (let ((fr-end (org-element-property :end (org-element-context))))
             (org-display-inline-images nil t fr-begin fr-end))))
    ))


(defun cw/org-auto-toggle-fragment-display ()
  "Automatically toggle a displayable org mode fragment"
  (and (eq 'org-mode major-mode)
       (let ((curr (cw/org-curr-fragment)))
         (cond
          ;; were on a fragment and now on a new fragment
          ((and
            ;; fragment we were on
            cw/org-last-fragment
            ;; and are on a fragment now
            curr
            ;; but not on the last one this is a little tricky. as you edit the
            ;; fragment, it is not equal to the last one. We use the begin
            ;; property which is less likely to change for the comparison.
            (not (equal curr cw/org-last-fragment)))

           ;; go back to last one and put image back, provided there is still a fragment there
           (save-excursion
             (cw/org-preview-fragment cw/org-last-fragment)
             ;; now remove current image
             (cw/org-remove-fragment-overlay curr)
             ;; and save new fragment
             )
           (setq cw/org-last-fragment curr))

          ;; were on a fragment and now are not on a fragment
          ((and
            ;; not on a fragment now
            (not curr)
            ;; but we were on one
            cw/org-last-fragment)
           ;; put image back on, provided that there is still a fragment here.
           (save-excursion
             (cw/org-preview-fragment cw/org-last-fragment))

           ;; unset last fragment
           (setq cw/org-last-fragment nil))

          ;; were not on a fragment, and now are
          ((and
            ;; we were not one one
            (not cw/org-last-fragment)
            ;; but now we are
            curr)
           ;; remove image
           (save-excursion
             (cw/org-remove-fragment-overlay curr)
             )
           (setq cw/org-last-fragment curr))

          ))))

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

(load "~/.emacs.d/private.el")
(add-hook 'prog-mode-hook #'code-stats-mode)
(add-hook 'org-mode-hook #'code-stats-mode)
(run-with-idle-timer 30 t #'code-stats-sync)
(add-hook 'kill-emacs-hook (lambda () (code-stats-sync :wait)))

(setq compilation-scroll-output 'first-error)

(add-to-list 'exec-path "~/.local/bin")

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

;; (define-minor-mode my-override-mode
;;   "Overrides all major and minor mode keys" t)

;; (defvar my-override-map (make-sparse-keymap "my-override-map")
;;   "Override all major and minor mode keys")

;; (add-to-list 'emulation-mode-map-alists
;;   `((my-override-mode . ,my-override-map)))

;; (define-key my-override-map (kbd "<left>")
;;   (lambda ()
;;     (interactive)
;;     (message "Use Vim keys: h for Left")))

;; (define-key my-override-map (kbd "<right>")
;;   (lambda ()
;;     (interactive)
;;     (message "Use Vim keys: l for Right")))

;; (define-key my-override-map (kbd "<up>")
;;   (lambda ()
;;     (interactive)
;;     (message "Use Vim keys: k for Up")))

;; (define-key my-override-map (kbd "<down>")
;;   (lambda ()
;;     (interactive)
;;     (message "Use Vim keys: j for Down")))
;; (evil-make-intercept-map my-override-map)

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
  "ca" 'lsp-execute-code-action
  "sl" 'lsp
  "ss" 'lsp-workspace-shutdown
  "sr" 'lsp-workspace-restart
  "wk" 'kill-buffer-and-window
  "wd" 'delete-window
  "ww" 'ace-window
  "gs" 'avy-goto-char-timer
  "gl" 'avy-goto-line
  "gr" 'revert-buffer
  "wr" 'hydra-window-resize-menu/body
  "pp" 'projectile-switch-project
  "pf" 'counsel-projectile-find-file
  "pc" 'projectile-compile-project
  "ff" 'counsel-find-file
  "cl" 'comment-or-uncomment-region
  "cf" 'counsel-grep-or-swiper
  "op" 'treemacs
  "om" 'magit
  "tt" 'treemacs-select-window)

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
