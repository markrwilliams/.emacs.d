;;; extras -- Miscellanious elisp
;;;
;;; Commentary:
;;; If possible don't use this!  Install a package instead
;;;
;;; Code:

(column-number-mode)

(defun find-next-string ()
  "Find the next string after the current one."
  (interactive)
  (let* ((stop (1+ (point-max)))
         (next-state (save-excursion (syntax-ppss (1+ (point)))))
         (string-is-next (eq (syntax-ppss-context next-state) 'string)))
    (when string-is-next
      (goto-char (scan-sexps (point) 1)))
    (while (and (< (point) stop)
                (not (eq 'string (syntax-ppss-context (syntax-ppss)))))
      (forward-char))
    (backward-char)))


(defun python-choose-string-literal (prefix)
  "Change a string literal's type.
PREFIX - (n)ative (b)ytes (u)nicode"
  (interactive "c(n)ative (b)ytes (u)nicode")
  (when (not (member prefix '(?n ?b ?u)))
    (user-error "%c not n b or u" prefix))
  (let ((end nil))
    (save-excursion
      (let ((state (syntax-ppss (1+ (point)))))
        (when (eq 'string (syntax-ppss-context state))
          (let* ((left (nth 8 state)))
            (goto-char left)
            (cond
             ((= prefix ?n)
              (when (or (= ?u (char-before))
                        (= ?b (char-before)))
                (delete-char -1)))
             ((not (= prefix (char-before)))
              (when (or (and (= prefix ?u) (= ?b (char-before)))
                        (and (= prefix ?b) (= ?u (char-before))))
                (delete-char -1))
              (insert-char prefix)))))))))


(defun python-query-string-literals ()
  "Interactively change string literals' types."
  (interactive)
  (let (done key)
    (while (not done)
      (find-next-string)
      (if (>= (point) (point-max))
          (setq done t)
        (progn
          (setq key (read-event "(n)ative (b)ytes (u)nicode (s)kip (q)uit"))
          (when (not (member key '(?n ?b ?u ?s ?q)))
            (user-error "%c not n b or u or s or q" key))
          (cond
           ((= key ?q) (setq done t))
           ((not (= key ?s))
            (python-choose-string-literal key)
            (forward-sexp))))))))


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


;; Rewrite footnotes crap

(defun mrw-rst-find-footnote (limit)
  (save-match-data
    (if (not (re-search-forward (rst-re 'ilm-pfx '(:grp fnc-tag "_") 'ilm-sfx) limit t))
        nil
      (let ((match-end (match-end 0)))
        (goto-char (match-beginning 1))
        (looking-at "\\[\\([0-9]\+\\)\\]")
        ;; assume footnotes start at 1 lol
        (if (string-to-number (match-string 1))
            (progn
              (goto-char match-end)
              (cons (match-string 1) (match-beginning 1)))
          (error "Not a footnote"))))))

(defun mrw-rst-find-footnote-definition (ref)
  (save-excursion
    (let (position)
    (while (and (not position) (re-search-forward (rst-re 'lin-beg 'fnc-sta-2) nil t))
        (goto-char (match-beginning 2))
        (save-match-data
          (looking-at "\\[\\([0-9]\+\\)\\]")
          (if (string= (match-string 1) ref)
              (setq position (match-beginning 1)))))
    position)))


(defun mrw-rst-find-footnote-cutoff ()
  (save-excursion
    (re-search-forward (rst-re 'lin-beg 'fnc-sta-2))
    (match-beginning 0)))


(defun mrw-rst-footnote-definition-pairs ()
  (save-excursion
    (let ((beginning-of-footnote-defs (mrw-rst-find-footnote-cutoff))
          (footnote-position nil)
          (footnotes nil)
          (pairs nil))
      (while (not (null (setq footnote-position (mrw-rst-find-footnote beginning-of-footnote-defs))))
        (let* ((footnote (car footnote-position))
               (position (cdr footnote-position))
               (definition-position (mrw-rst-find-footnote-definition footnote)))
          (when (and definition-position (not (member footnote footnotes)))
            (add-to-list 'footnotes footnote)
            (add-to-list 'pairs (cons position definition-position) t))))
      pairs)))

(defun mrw-rst-rewrite-footnote-numbers (pairs)
  (save-excursion
    (let ((footnote (length pairs)))
      (dolist (footnote-definition (nreverse pairs))
        (let ((footnote-position (car footnote-definition))
              (footnote-definition-position (cdr footnote-definition)))
          (goto-char footnote-definition-position)
          (zap-to-char nil ?\])
          (insert (number-to-string footnote) "]")
          (goto-char footnote-position)
          (zap-to-char nil ?\])
          (insert (number-to-string footnote) "]")
          (setq footnote (1- footnote)))))))


(defun rst-rewrite-footnotes ()
  "Number footnotes in a reStructuredText file numbered by position.
The footnotes must be uniquely identified"
  (interactive)
  (mrw-rst-rewrite-footnote-numbers (mrw-rst-footnote-definition-pairs)))

;; End rewrite footnotes crap.

;;; extras.el ends here
