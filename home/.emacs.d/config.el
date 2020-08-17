(defun init-hooks () (global-display-line-numbers-mode 1))

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(setq dashboard-items '((recents  . 5)
                        ;(bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

(add-hook 'after-init-hook 'init-hooks)

(defun kill-buffer-if-exists (buffer)
  (when (not (eq nil (get-buffer buffer)))
    (delete-windows-on buffer) (kill-buffer buffer)))

(defun kill-compilation-buffer ()
  (interactive)
  (kill-buffer-if-exists "*compilation*"))

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
                    :family "Hack"
                    :height 85)

(setq company-math-allow-latex-symbols-in-faces t)

;; isearch
(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
(setq case-fold-search t)

;; Org-mode
(setq org-hide-emphaisi-markers t)
(add-hook 'org-mode-hook 
          (lambda () 
            (org-bullets-mode 1)
            (org-indent-mode 1)))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-agenda-files '("~/org"))
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
