(defconst lama-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; C/C++ style comments
	(modify-syntax-entry ?- ". 124b")
	;; (modify-syntax-entry ?* ". 23")
	(modify-syntax-entry ?\n "> b")
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"")
    (syntax-table))
  "Syntax table for `lama'.")

(eval-and-compile
  (defconst lama-keywords
    '("after" "array" "at" "before" "box" "elif" "eta" "false" "infix" "infixl" "infixr" "lazy" "sexp" "skip" "str" "syntax" "then" "true" "val" "while" "if" "fi" "else" "for" "let" "import" "var" "fun" "do" "od" "public" "case" "of" "esac" )))

(defconst lama-highlights
  `((,(regexp-opt lama-keywords 'symbols) . font-lock-keyword-face)))

;;;###autoload
(define-derived-mode lama-mode prog-mode "lama"
  "Major Mode for editing Lama source code."
  :syntax-table lama-mode-syntax-table
  (setq font-lock-defaults '(lama-highlights))
  (setq-local comment-start "-- "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lama\\'" . lama-mode))

(provide 'lama-mode)
