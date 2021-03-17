(defun load-org ()
  (progn 
    (require 'org)
    (setq org-agenda-files '("~/Dropbox/org"))))

(load-org) ; Loading org options on load

(defun batch-all-agenda () (eval '(org-batch-agenda-csv "a")))

(defun parse-study-with (f)
  (progn
    (find-file "~/Dropbox/org/Study.org")
    (org-element-map (org-element-parse-buffer) 'headline f)))

(defun get-study-headline (s)
  (parse-study-with
    (lambda (h) 
      (let ((title (org-element-property :raw-value h))
            (parent (org-element-property :parent h)))
        (if (string= title s)
          (princ (org-element-interpret-data parent)))))))

(defun study-mark-done (s)
  (parse-study-with
    (lambda (h) 
      (let ((title (org-element-property :raw-value h)))
        (if (string= title s)
          (org-todo 'done))) h)))
; (defun study-mark-done (s)
;   (org-map-entries
;     (lambda ()
;       (let* ((h (org-element-at-point))
;              (title (org-element-property :raw-value h)))
;       (if (string= title s) (org-todo 'done)))) nil '("~/Dropbox/org/Study.org")))
