
(general-define-key
  "M-x" 'counsel-M-x)

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
  "ww" 'ace-window
  "wr" 'hydra-window-resize-menu/body
  "pp" 'counsel-projectile-switch-project
  "pf" 'counsel-projectile-find-file
  "ff" 'counsel-find-file
  "cl" 'comment-or-uncomment-region
  "cf" 'counsel-grep-or-swiper
  "op" 'treemacs)


;; For all other keybindings
(general-define-key
  :states '(normal visual emacs insert treemacs)
  :keymaps 'override
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

