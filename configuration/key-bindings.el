;;; key-bindings -- key bindings
;;;
;;; Commentary:
;;; This is where global key bindings live
;;;
;;; Code:

(global-set-key [?\C-h] 'backward-delete-char-untabify)
(global-set-key [?\C-x ??] 'help-command)
;; makes C-x p do C-x o backwards
(define-key global-map "\C-xp" '(lambda () (interactive) (other-window -1)))

;;; key-bindings.el ends here
