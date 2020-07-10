
(defvar my-leader-map (make-sparse-keymap)
  "Keymap for \"leader key\" shortcuts.")

;; binding "SPC" to the keymap
(define-key evil-normal-state-map (kbd "SPC") my-leader-map)


;; Example
;;(defun dbg-message ( msg ) (message-box (concat "DBG: " msg)))
;;(defun dbg-message-leader () (interactive) (dbg-message "Test leader"))
;(add-hook 'after-init-hook (dbg-message "loading complete"))
;(evil-leader/set-key
  ;"dd" (dbg-message "Test evil-leader")) 
;;(define-key my-leader-map (kbd "dd") 'dbg-message-leader)



