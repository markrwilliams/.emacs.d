;;; packages -- Ensure and configure all the packages
;;;
;;; Commentary:
;;; Install and configure packages here via use-packages
;;;
;;; Code:


(package-initialize)

;; bootstrap
(when (not (fboundp 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))


(use-package flycheck
  :config (global-flycheck-mode)
  :ensure t)

;; helm
;; jedi can use helm; make helm more reasonable
;; TODO: can helm's run grep be made to actually behave like grep?
(use-package helm
  :config (progn
            ;; http://tuhdo.github.io/helm-intro.html
            (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
                  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
                  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
                  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
                  helm-ff-file-name-history-use-recentf t
                  helm-echo-input-in-header-line t)

            (setq helm-autoresize-max-height 0)
            (setq helm-autoresize-min-height 20)
            (helm-autoresize-mode 1)

            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
            (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
            (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
            (define-key helm-map (kbd "C-c p")  'helm-previous-source)
            (define-key helm-map (kbd "C-c n")  'helm-next-source)

            (global-set-key (kbd "M-y") 'helm-show-kill-ring)
            (global-set-key (kbd "C-x b") 'helm-mini)
            (global-set-key (kbd "C-c h o") 'helm-occur)

            )
  :ensure t)

;; python python python
(use-package adaptive-wrap :ensure t)
(use-package python-docstring :ensure t)
(use-package pyvenv
  :config (pyvenv-tracking-mode)
  :ensure t )
(use-package auto-virtualenv :ensure t)
(use-package column-marker :ensure t)
(use-package jedi
  :config (jedi:install-server)
  :ensure t)
(use-package python
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error)
         ("C-c C-w" . pyvenv-workon)
         ("C-c C-d" . pyvenv-deactivate))
  :config (progn
            ;; swap quotes
            (add-hook 'python-mode-hook (lambda ()
                                          (local-unset-key "\C-c\C-s")
                                          (local-set-key "\C-c\C-s" 'python-swap-quotes)))
            (python-docstring-install)
            ;; auto activate virtualenvs in pyvenv
            (add-hook 'python-mode-hook (lambda () (ignore-errors (auto-virtualenv-set-virtualenv))))
            ;; () [] {} '' ""
            (add-hook 'python-mode-hook 'electric-pair-mode)
            ;; wrap stupidly long lines
            (add-hook 'python-mode-hook (lambda ()
                                          (adaptive-wrap-prefix-mode t)
                                          (setf adaptive-wrap-extra-indent 2)))
            (add-hook 'python-mode-hook (lambda ()
                                          (when (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)
                                            (set (make-local-variable 'jedi:server-args)
                                                 `("--virtual-env" ,pyvenv-virtual-env)))
                                          (jedi:setup)))
            ))


;;; packages.el ends here
