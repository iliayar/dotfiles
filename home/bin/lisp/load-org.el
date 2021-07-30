(defun load-org ()
  (progn 
    (require 'org)
    (setq org-agenda-files '("~/org"))))

(load-org) ; Loading org options on load

(defun batch-all-agenda () (eval '(org-batch-agenda-csv "a" org-agenda-span 'week)))

(defun with-file (file f)
  (progn
    (find-file (format "~/org/%s.org" file))
    (org-element-map (org-element-parse-buffer) 'headline f)))

(defun with-study (f)
  (with-file "Study" f))

(defun find-headline (file s)
  (car (with-file file
    (lambda (h) 
      (let ((title (org-element-property :raw-value h)))
        (if (string= title s) h))))))

;; (princ (with-current-buffer (org-html-export-as-html (find-headline "Notes" "Rust Agenda")) (buffer-string)))
;; (find-headline "Notes" "Rust Agenda" (lambda (h) (princ (with-current-buffer (org-ascii-export-as-ascii) (buffer-string)))))
(princ (progn
	 (find-headline "Notes" "Rust Agenda")
	 (org-html-export-as-html nil t nil t)
	 (with-current-buffer (get-buffer "*Org HTML Export*") (buffer-string))))
	 

(defun get-headline (file s)
  (parse-with file
    (lambda (h) 
      (let ((title (org-element-property :raw-value h)))
        (if (string= title s)
          (princ (org-element-interpret-data h)))))))

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
        (if (string= title s) (org-todo 'done)))) nil (list (format "~/org/%s.org" file))))
(defun mark-done (s file)
  (progn (find-file (format "~/org/%s.org" file))
         (with-current-buffer (find-buffer-visiting (format "~/org/%s.org" file))
                              (progn
                                (mark--done s file)
                                (save-buffer)))))
