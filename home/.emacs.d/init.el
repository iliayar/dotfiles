(setq comp-deferred-compilation t)
(org-babel-load-file "~/.emacs.d/config.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:output-area-inlined-images t)
 '(mu4e-completing-read-function 'ivy-completing-read)
 '(org-capture-bookmark nil)
 '(org-display-remote-inline-images 'cache)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-roam-completion-everywhere t)
 '(package-selected-packages
    '(w3m org-roam ox-rss nim-mode jupyter ein nlinum-relative elfeed-org elfeed-goodies elfeed lsp-ivy gcmh haskell-lsp evil-mu4e list-packages-ext mu4e smooth-scroll smooth-scrolling rustic rust-mode org-alert tramp-hdfs gnuplot atom-one-dark-theme lsp-haskell presentation highlight-doxygen lsp-java lsp-pyright cmake-mode cpputils-cmake cmake-ide org-pomodoro org-special-block-extras company-ctags ox-reveal org-super-agenda code-stats lsp-intellij dockerfile-mode docker ob-ipython graphviz-dot-mode highlight-indentation vterm latex-pretty-symbols pretty-symbols web-mode zzz-to-char dtrt-indent anaconda-mode company-anaconda unicode-fonts gradle-mode kotlin-mode origami ewal-doom-themes ewal impatient-mode company-lsp format-all yaml-mode ox-md hl-todo evil-multiedit treemacs-all-the-icons evil-collection use-package))
 '(smtpmail-smtp-server "smtp.google.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1c1e1f" :foreground "#d6d6d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 86 :width normal :foundry "CTDB" :family "Fira Code"))))
 '(italic ((t (:slant italic :family "Hack"))))
 '(org-block ((t (:extend t :background "gray10"))))
 '(org-block-begin-line ((t (:extend t :background "#1c1e1f" :foreground "#555556" :overline nil :underline t))))
 '(org-block-end-line ((t (:inherit org-block-begin-line :extend t :overline t :underline nil))))
 '(org-document-title ((t (:foreground "#fd971f" :weight bold :height 1.3))))
 '(org-ellipsis ((t (:foreground "red"))))
 '(org-footnote ((t (:foreground "#fd971f" :weight extra-bold :height 0.7))))
 '(org-latex-and-related ((t (:inherit nil :foreground "tomato" :weight bold))))
 '(org-level-1 ((t (:inherit outline-1 :extend t :underline t :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :extend t :height 1.1))))
 '(org-link ((t (:foreground "deep sky blue" :inherit link))))
 '(org-tag ((t (:foreground "#e2c770" :slant italic :weight normal :family "Hack"))))
 '(org-verbatim ((t (:foreground "#b6e63e" :box (:line-width (2 . 2) :color "dim gray" :style released-button)))))
 '(outline-1 ((t (:extend t :foreground "#fb2874" :weight bold)))))
