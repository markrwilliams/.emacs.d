;;; init.el -- Entry
;;;
;;; Commentary:
;;; Emacs starts here
;;;
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun mrw-configuration-path (filename)
  "The path to configuration FILENAME."
  (concat (file-name-as-directory user-emacs-directory)
          (file-name-as-directory "configuration")
          filename))

(load-file (mrw-configuration-path "contain-customize.el"))
(load-file (mrw-configuration-path "tls-trust.el"))
(load-file (mrw-configuration-path "switch-panel.el"))
(load-file (mrw-configuration-path "notabs.el"))

(load-file (mrw-configuration-path "extras.el"))
(load-file (mrw-configuration-path "packages.el"))
(load-file (mrw-configuration-path "key-bindings.el"))
(load-file (mrw-configuration-path "hooks.el"))

;;; init.el ends here
