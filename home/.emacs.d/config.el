(defun init-hooks () (global-display-line-numbers-mode 1))

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

(add-hook 'after-init-hook 'init-hooks)

(defvar my-leader-map (make-sparse-keymap)
  "Keymap for \"leader key\" shortcuts.")

;; binding "SPC" to the keymap
(define-key evil-normal-state-map (kbd "SPC") my-leader-map)


;; Example
;(defun dbg-message () (interactive) (message-box (format "%s" (nth 0 [1 2 3]))))
;(add-hook 'after-init-hook (dbg-message "loading complete"))
;(evil-leader/set-key
  ;"dd" (dbg-message "Test evil-leader")) 
;(define-key my-leader-map (kbd "dbg") 'dbg-message)


(defun kill-buffer-if-exists (buffer)
  (when (not (eq nil (get-buffer buffer)))
    (delete-windows-on buffer) (kill-buffer buffer))
  )

(defun kill-compilation-buffer ()
  (interactive)
  (kill-buffer-if-exists "*compilation*")
  )

(defun map! (mode maps) 
  (seq-map (lambda (key-fnc) 
             (define-key mode (kbd (format "%s" (elt key-fnc 0))) (elt key-fnc 1))) 
           maps))

(map! my-leader-map 
  [["cd" kill-compilation-buffer]
   ["cl" comment-or-uncomment-region]
   ["cc" compile]
   ["wk" kill-buffer-and-window]
   ["wd" delete-window]
   ["pp" counsel-projectile-switch-project]
   ["ff" counsel-projectile-find-file]
   ["op" treemacs]])

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

;; set font Hack 15 pt
(set-face-attribute 'default nil
                    :family "Hack Nerd Font Mono"
                    :height 85)

;; set my init filt to be this file
(setq user-init-file "~/.emacs.d/init.el")
