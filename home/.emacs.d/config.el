(defun init-hooks () (global-display-line-numbers-mode 1))

(setq dashboard-items '((recents  . 5)
                        ;(bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

(add-hook 'after-init-hook 'init-hooks)

(defun kill-buffer-if-exists (buffer)
  (when (not (eq nil (get-buffer buffer)))
    (delete-windows-on buffer) (kill-buffer buffer))
  )

(defun kill-compilation-buffer ()
  (interactive)
  (kill-buffer-if-exists "*compilation*"))

(general-create-definer my-leader-def
  :prefix "SPC")

(my-leader-def
  :states '(normal visual)
  "cl" 'comment-or-uncomment-region)


(my-leader-def
  :states 'normal
  "cd" 'kill-compilation-buffer
  "cc" 'compile
  "cr" 'lsp-rename
  "sl" 'lsp
  "wk" 'kill-buffer-and-window
  "wd" 'delete-window
  "pp" 'counsel-projectile-switch-project
  "ff" 'counsel-find-file
  "op" 'treemacs)

(scroll-bar-mode 0) ; no scroll bar
(tool-bar-mode 0) ; no tool bar
(menu-bar-mode 0) ; no menu bar
(show-paren-mode 1) ; visualize matching parenthesees
(global-hl-line-mode 1) ; highlight current line
(eldoc-mode 1) ; enable docs in minibuffer
;; (setq inhibit-startup-screen 1) ; no start screen

;; store all backups in a single directory 
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; y or n instead of yes-or no
(fset 'yes-or-no-p 'y-or-n-p)

;; no annoying bell!
(setq ring-bell-function 'ignore)

;; set font
(set-face-attribute 'default nil
                    :family "Hack Nerd Font Mono"
                    :height 85)

(setq company-math-allow-latex-symbols-in-faces t)

;; set my init filt to be this file
(setq user-init-file "~/.emacs.d/init.el")
