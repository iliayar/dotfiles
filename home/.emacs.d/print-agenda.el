(org-eval-in-environment (org-make-parameter-alist
			  `(org-agenda-span
			    'week
			    org-agenda-use-time-grid t
			    org-agenda-remove-tags t
			    org-agenda-window-setup 'nope))
   (let* ((wins (current-window-configuration))
 	 org-agenda-sticky)
     (save-excursion
      (with-current-buffer
	  (get-buffer-create org-agenda-buffer-name)
	 (pop-to-buffer (current-buffer))
	 (org-agenda nil "a")
        (let ((result (buffer-string)))
	  (with-temp-file "~/.agenda" (insert result)))))
     (set-window-configuration wins)))
