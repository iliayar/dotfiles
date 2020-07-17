
(general-define-key
  :states '(normal visual emacs insert treemacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :keymaps 'override
  "cd" 'kill-compilation-buffer
  "cc" 'compile
  "cr" 'lsp-rename
  "sl" 'lsp
  "wk" 'kill-buffer-and-window
  "wd" 'delete-window
  "ww" 'evil-window-next
  "pp" 'counsel-projectile-switch-project
  "ff" 'counsel-find-file
  "cl" 'comment-or-uncomment-region
  "op" 'treemacs)


;; For all other keybindings
(general-define-key
  :states '(normal visual emacs insert treemacs)
  :keymaps 'override
  )
