(defun study-mark-done (s)
  (org-map-entries
    (lambda ()
      (let* ((h (org-element-at-point))
             (title (org-element-property :raw-value h)))
      (if (string= title s) (org-todo 'done)))) nil '("~/org/Study.org")))
