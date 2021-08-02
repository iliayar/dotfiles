;; It takes time
;; (require 'package)
;; (package-initialize)
;; (require 'ox-json)
(require 'org)
(require 'org-element)
(setq org-agenda-files '("~/org"))

(defun batch-all-agenda () (eval '(org-batch-agenda-csv "a"
							org-agenda-span 14
							org-agenda-skip-scheduled-if-done t
							org-agenda-skip-deadline-if-done t
							org-agenda-skip-deadline-prewarning-if-scheduled t
							org-agenda-entry-text-mode t)))
(defun with-file (file fun)
  (progn
    (find-file (format "~/org/%s.org" file))
    (org-element-map (org-element-parse-buffer) 'headline fun)))

(defun with-study (fun)
  (with-file "Study" fun))

(defun find-headline (file headline)
  (car (with-file file
		  (lambda (h) 
		    (let ((title (org-element-property :raw-value h)))
		      (if (string= title headline) h))))))

(defun find-headline-parent (file headline)
  (org-element-property :parent (find-headline file headline)))

(defun goto-element (elem)
  (goto-char (org-element-property :begin elem)))

(defun export-element-with (elem exporter)
  "
EXPORTER - The org export function, e.g. (lambda () (org-html-export-as-html nil t nil t))
"
  (progn
    (goto-element elem)
    (with-current-buffer (funcall exporter) (buffer-string))))

(defun export-headline-with (file headline exporter)
    (export-element-with (find-headline file headline) exporter))

(defun export-headline-parent-with (file headline exporter)
    (export-element-with (find-headline-parent file headline) exporter))

(defun princ-headline-with (file headline exporter)
  (princ (export-headline-with file headline exporter)))

(defun princ-headline-parent-with (file headline exporter)
  (princ (export-headline-parent-with file headline exporter)))

(defun get-study-headline (headline)
  (princ-headline-parent-with "Study" headline 'org-org-export-as-org))

(defun mark-done (file headline)
  (progn
    (goto-element (find-headline file headline))
    (org-todo 'done)
    (save-buffer)))
