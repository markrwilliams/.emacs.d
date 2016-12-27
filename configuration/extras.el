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


(defun python-twistedchecker-error-filter (errors)
  "The error filter for python-twistedchecker.  ERRORS from flycheck."
  (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))

(defun establish-python-twistedchecker ()
  "Teach flycheck about twistedchecker - thanks, idnar!"
  (flycheck-define-checker python-twistedchecker
    "A Python syntax and style checker using twistedchecker."
    :command ("twistedchecker"
              ;; Need `source-inplace' for relative imports (e.g. `from .foo
              ;; import bar'), see https://github.com/flycheck/flycheck/issues/280
              source-inplace)
    :error-filter python-twistedchecker-error-filter
    :error-patterns
    ((error line-start (or "E" "F") (id (one-or-more (not (any ":")))) ":"
            (zero-or-more " ") line "," column ":" (message) line-end)
     (warning line-start (or "W" "R" "C") (id (one-or-more (not (any ":")))) ":"
              (zero-or-more " ") line "," column ":" (message) line-end))
    :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-twistedchecker 'append)

  ;; ;; Chain to another checker (I use flake8, for example)
  (flycheck-add-next-checker 'python-twistedchecker 'python-pyflakes))

;; ;; Activate this checker for all Python files (you probably don't want this
;; ;; unless you only work on Twisted)
;; (add-hook 'python-mode-hook
;;           (lambda () (flycheck-select-checker 'python-twistedchecker)))


;;; extras.el ends here
