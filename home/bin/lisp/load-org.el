(defun load-org ()
  (progn 
    (require 'org)
    (setq org-agenda-files '("~/Dropbox/org"))))

(load-org) ; Loading org options on load

(defun batch-all-agenda () (eval '(org-batch-agenda-csv "a" org-agenda-span 'day)))

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

(defun mark--done (s file)
  (org-map-entries
    (lambda ()
      (let* ((h (org-element-at-point))
             (title (org-element-property :raw-value h)))
        (if (string= title s) (org-todo 'done)))) nil (list (format "~/Dropbox/org/%s.org" file))))
(defun mark-done (s file)
  (progn (find-file (format "~/Dropbox/org/%s.org" file))
         (with-current-buffer (find-buffer-visiting (format "~/Dropbox/org/%s.org" file))
                              (progn
                                (mark--done s file)
                                (save-buffer)))))
