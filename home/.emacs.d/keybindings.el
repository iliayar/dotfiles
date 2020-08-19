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
  "wk" 'kill-buffer-and-window
  "wd" 'delete-window
  "ww" 'ace-window
  "wr" 'hydra-window-resize-menu/body
  "pp" 'projectile-switch-project
  "pf" 'counsel-projectile-find-file
  "ff" 'counsel-find-file
  "cl" 'comment-or-uncomment-region
  "cf" 'counsel-grep-or-swiper
  "op" 'treemacs
  "om" 'magit)


;; For all other keybindings
(general-define-key
  :states '(normal visual)
  :prefix "M-g"
  :keymaps 'override
  "s" 'avy-goto-char-timer
  "l" 'avy-goto-line
  )

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

