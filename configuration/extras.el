;;; extras -- Miscellanious elisp
;;;
;;; Commentary:
;;; If possible don't use this!  Install a package instead
;;;
;;; Code:

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun python-swap-quotes ()
  "Swap single and double quotes."
  (interactive)
  (save-excursion
    (let ((state (syntax-ppss)))
      (when (eq 'string (syntax-ppss-context state))
        (let* ((left (nth 8 state))
               (right (1- (scan-sexps left 1)))
               (newquote (if (= ?' (char-after left))
                             ?\" ?')))
          (dolist (loc (list left right))
            (goto-char loc)
            (delete-char 1)
            (insert-char newquote 1)))))))

;;; extras.el ends here
